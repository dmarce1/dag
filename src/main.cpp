#include <memory>
#include <vector>
#include <complex>
#include <unordered_set>
#include <stack>
#include <set>
#include <deque>
#include <cassert>
#include <unordered_map>

class dag;
class node;

using node_ptr = std::shared_ptr<node>;

#define tiny 1e-14

enum node_type {
	ADD, SUB, MUL, FMA, NEG, IN, CON, OUT, ASN
};

struct instruction_t {
	node_type op;
	std::vector<std::string> vars;
};

void print_instructions(const std::vector<instruction_t>& ins) {
	std::set<std::string> declared;
	for (unsigned i = 0; i < ins.size(); i++) {
		const auto var = ins[i].vars[0];
		if (declared.find(var) == declared.end()) {
			declared.insert(var);
			if (var[0] == 'r') {
				printf("\tdouble %s;\n", var.c_str());
			}
		}
		printf("\t%s = ", var.c_str());
		if (ins[i].op == FMA) {
			printf("std::fma(%s, %s, %s);\n", ins[i].vars[1].c_str(), ins[i].vars[2].c_str(), ins[i].vars[3].c_str());
		} else {
			if (ins[i].op == NEG) {
				printf("-");
			}
			printf("%s", ins[i].vars[1].c_str());
			switch (ins[i].op) {
			case ADD:
				printf(" + ");
				break;
			case SUB:
				printf(" - ");
				break;
			case MUL:
				printf(" * ");
				break;
			default:
				break;
			};
			if (ins[i].op != NEG && ins[i].op != OUT && ins[i].op != ASN) {
				printf("%s;\n", ins[i].vars[2].c_str());
			} else {
				printf(";\n", ins[i].op);
			}
		}
	}
}

class node {
	node_type type;
	std::vector<node_ptr> input;
	std::weak_ptr<node> self_ptr;
	double value;
	bool done;
	std::string name;
	template<class ...Args>
	void add_input(node_ptr a) {
		auto my_ptr = std::make_shared<node>();
		auto other_ptr = std::make_shared<node>();
		my_ptr = a;
		other_ptr = node_ptr(self_ptr);
		input.push_back(my_ptr);
	}
	template<class ...Args>
	void add_input(node_ptr a, Args ...others) {
		add_input(a);
		add_input(others...);
	}
	std::vector<node_ptr> list(std::unordered_set<node_ptr>& touched) const {
		std::vector<node_ptr> nodes;
		if (touched.find(node_ptr(self_ptr)) == touched.end()) {
			touched.insert(node_ptr(self_ptr));
			for (auto i : input) {
				auto tmp = i->list(touched);
				nodes.insert(nodes.end(), tmp.begin(), tmp.end());
			}
			nodes.push_back(node_ptr(self_ptr));
		}
		return nodes;
	}
public:
	~node() {
	}
	bool equivalent(node_ptr other, bool term = false) {
		bool rc;
		rc = false;
		for (int i = 0; i < 2; i++) {
			if (type == CON) {
			} else {
				if (type == other->type) {
					rc = true;
					for (unsigned i = 0; i < input.size(); i++) {
						if (input[i]->type == CON) {
							if (other->input[i]->type == CON) {
								if (std::abs(other->input[i]->value - input[i]->value) > tiny) {
									rc = false;
								}
							} else {
								rc = false;
							}
						} else if (input[i].get() != other->input[i].get()) {
							rc = false;
						}

					}

				}
			}
			if (i == 0 && !rc && input.size() > 1) {
				std::swap(input[0], input[1]);
			}
			if (i == 1 && input.size() > 1) {
				std::swap(input[0], input[1]);
			}
			if (rc) {
				break;
			}
		}
		return rc;
	}
	bool zero() const {
		return type == CON && std::abs(value) < tiny;
	}
	bool one() const {
		return type == CON && std::abs(value - 1.0) < tiny;
	}
	bool none() const {
		return type == CON && std::abs(value + 1.0) < tiny;
	}
	friend class dag;
	static node_ptr create(node_type op) {
		auto ptr = std::make_shared<node>();
		ptr->type = op;
		ptr->self_ptr = ptr;
		ptr->done = false;
		return ptr;
	}
	template<class ...Args>
	static node_ptr create(node_type op, Args ... others) {
		auto ptr = create(op);
		ptr->add_input(others...);
		return ptr;
	}
	friend node_ptr operator+(node_ptr a, node_ptr b);
	friend node_ptr operator-(node_ptr a, node_ptr b);
	friend node_ptr operator*(node_ptr a, node_ptr b);
	friend node_ptr operator-(node_ptr a);
	friend node_ptr constant(double a);
	friend node_ptr negative_constant(double a);
	friend node_ptr create_input(int i);
	friend node_ptr fma(node_ptr a, node_ptr b, node_ptr c);
};

node_ptr constant(double a) {
	if (a < 0.0) {
		return -constant(-a);
	} else {
		auto ptr = node::create(CON);
		ptr->value = a;
		return ptr;
	}
}

node_ptr negative_constant(double a) {
	auto ptr = node::create(CON);
	ptr->value = a;
	return ptr;
}

node_ptr fma(node_ptr a, node_ptr b, node_ptr c) {
	return node::create(FMA, a, b, c);
}

node_ptr create_input(int i) {
	auto ptr = node::create(IN);
	ptr->name = std::string("x[") + std::to_string(i) + std::string("]");
	ptr->done = true;
	return ptr;
}

node_ptr operator+(node_ptr a, node_ptr b) {
	return node::create(ADD, a, b);
}

node_ptr operator-(node_ptr a, node_ptr b) {
	return node::create(SUB, a, b);
}

node_ptr operator*(node_ptr a, node_ptr b) {
	return node::create(MUL, a, b);
}

node_ptr operator-(node_ptr a) {
	return node::create(NEG, a);
}

class dag {
	std::vector<node_ptr> inputs;
	std::vector<node_ptr> outputs;
public:
	std::vector<std::weak_ptr<node>> list() const {
		std::vector<std::weak_ptr<node>> nodes;
		std::unordered_set<node_ptr> touched;
		for (unsigned i = 0; i < outputs.size(); i++) {
			auto tmp = outputs[i]->list(touched);
			nodes.insert(nodes.end(), tmp.begin(), tmp.end());
		}
		return nodes;
	}
	std::vector<node_ptr>& get_inputs(int N) {
		for (int i = 0; i < N; i++) {
			inputs.push_back(create_input(i));
		}
		return inputs;
	}
	void set_outputs(std::vector<node_ptr>&& outs) {
		outputs = std::move(outs);
		for (unsigned i = 0; i < outputs.size(); i++) {
			outputs[i] = node::create(OUT, outputs[i]);
			outputs[i]->name = std::string("x[") + std::to_string(i) + std::string("]");
		}
	}
	void optimize_fma() {
		auto nodes = list();
		for (auto wnode : nodes) {
			if (wnode.use_count() == 0) {
				continue;
			}
			auto node = node_ptr(wnode);
			node_ptr a, b, c;
			if (node->type == ADD) {
				if (node->input[1]->type == MUL) {
					a = node->input[1]->input[0];
					b = node->input[1]->input[1];
					c = node->input[0];
				} else if (node->input[0]->type == MUL) {
					a = node->input[0]->input[0];
					b = node->input[0]->input[1];
					c = node->input[1];
				}
				if (a != nullptr) {
					node->input.resize(0);
					node->input.push_back(a);
					node->input.push_back(b);
					node->input.push_back(c);
					node->type = FMA;
				}
			} else if (node->type == SUB) {
				if (node->input[1]->type == MUL) {
					if (node->input[1]->input[0]->type == CON) {
						a = negative_constant(-node->input[1]->input[0]->value);
						b = node->input[1]->input[1];
						c = node->input[0];
					} else if (node->input[1]->input[1]->type == CON) {
						a = negative_constant(-node->input[1]->input[1]->value);
						b = node->input[1]->input[0];
						c = node->input[0];
					}
					if (a != nullptr) {
						auto node = fma(a, b, c);
					}
				} else if (node->input[0]->type == MUL) {
					if (node->input[0]->input[0]->type == CON) {
						a = negative_constant(-node->input[0]->input[0]->value);
						b = node->input[0]->input[1];
						c = node->input[1];
					} else if (node->input[0]->input[1]->type == CON) {
						a = negative_constant(-node->input[0]->input[1]->value);
						b = node->input[0]->input[0];
						c = node->input[1];
					}
					if (a != nullptr) {
						auto fma_node = fma(a, b, c);
						node->type = NEG;
						node->input.resize(0);
						node->input.push_back(fma_node);
					}
				}

			}
		}
	}

	void optimize_common_subexps() {
//		return;
		auto nodes = list();
		std::unordered_map<node*, node_ptr> node_map;
		for (auto wnode : nodes) {
			if (wnode.use_count() == 0) {
				continue;
			}

			auto node = node_ptr(wnode);
			node_ptr ptr = nullptr;
			if (node->type == ADD || node->type == SUB || node->type == MUL || node->type == NEG) {
				for (auto i = node_map.begin(); i != node_map.end(); i++) {
					if (i->second->equivalent(node)) {
						ptr = i->second;
						break;
					}
				}
				if (ptr) {
					node_map[node.get()] = ptr;
				} else {
					node_map[node.get()] = node;
				}
			}
		}
		for (auto wnode : nodes) {
			if (wnode.use_count() == 0) {
				continue;
			}

			auto node = node_ptr(wnode);
			for (auto & in : node->input) {
				if (in->type == ADD || in->type == SUB || in->type == MUL || in->type == NEG) {
					in = node_map[in.get()];
				}
			}
		}
	}
	void propagate_signs() {
		bool rc;
		rc = true;
		while (rc) {
			rc = false;
			auto nodes = list();
			for (auto wnode : nodes) {
				if (wnode.use_count() == 0) {
					continue;
				}

				auto node = node_ptr(wnode);
				switch (node->type) {
				case MUL:
					if (node->input[0]->type == NEG && node->input[1]->type == NEG) {
						node->input[0] = node->input[0]->input[0];
						node->input[1] = node->input[1]->input[0];
						rc = true;
					} else if (node->input[0]->type == NEG) {
						auto tmp = *node;
						node->input[0] = node->input[0]->input[1] * node->input[1];
						node->input[0]->done = tmp.done;
						node->input[0]->name = tmp.name;
						node->input.resize(1);
						node->type = NEG;
						rc = true;
					} else if (node->input[1]->type == NEG) {
						auto tmp = *node;
						node->input[0] = node->input[1]->input[0] * node->input[0];
						node->input[0]->done = tmp.done;
						node->input[0]->name = tmp.name;
						node->input.resize(1);
						node->type = NEG;
						rc = true;
					}
					break;
				case ADD:
					if (node->input[0]->type == NEG) {
						node->type = SUB;
						node->input[0] = node->input[0]->input[0];
						std::swap(node->input[0], node->input[1]);
						rc = true;
					} else if (node->input[1]->type == NEG) {
						node->type = SUB;
						node->input[1] = node->input[1]->input[0];
						rc = true;
					} else if (node->input[0]->type == NEG && node->input[1]->type == NEG) {
						auto new_node = node->input[0]->input[0] + node->input[1]->input[0];
						node->type = NEG;
						node->input[0] = new_node;
						node->input.resize(1);
						rc = true;
					}
					break;
				case SUB:
					if (node->input[0]->type == NEG) {
						auto tmp = *node;
						node->input[0] = node->input[0]->input[0] + node->input[1];
						node->input[0]->done = tmp.done;
						node->input[0]->name = tmp.name;
						node->input.resize(1);
						node->type = NEG;
						rc = true;
					} else if (node->input[1]->type == NEG) {
						node->type = ADD;
						node->input[1] = node->input[1]->input[0];
						rc = true;
					} else if (node->input[0]->type == NEG && node->input[1]->type == NEG) {
						node->type = ADD;
						node->input[0] = node->input[0]->input[0];
						node->input[1] = node->input[1]->input[0];
						std::swap(node->input[0], node->input[1]);
						rc = true;
					}
					break;
				default:
					break;
				}
			}
		}
	}

	void optimize_constants() {
		bool rc;
		rc = true;
		while (rc) {
			rc = false;
			auto nodes = list();
			for (auto wnode : nodes) {
				if (wnode.use_count() == 0) {
					continue;
				}

				auto node = node_ptr(wnode);
				switch (node->type) {
				case MUL:
					if (node->input[0]->zero() || node->input[1]->zero()) {
						node->value = 0.0;
						node->name = std::to_string(0);
						node->type = CON;
						node->input.resize(0);
						rc = true;
					} else if (node->input[0]->one()) {
						node->value = node->input[1]->value;
						node->name = node->input[1]->name;
						node->type = node->input[1]->type;
						node->input = node->input[1]->input;
						rc = true;
					} else if (node->input[1]->one()) {
						node->value = node->input[0]->value;
						node->name = node->input[0]->name;
						node->type = node->input[0]->type;
						node->input = node->input[0]->input;
						rc = true;
					} else if (node->input[0]->none()) {
						node->input[0] = node->input[1];
						node->input.resize(1);
						node->type = NEG;
						rc = true;
					} else if (node->input[1]->none()) {
						node->input.resize(1);
						node->type = NEG;
						rc = true;
					} else if (node->input[0]->type == NEG && node->input[1]->type == NEG) {
						node->input[0] = node->input[0]->input[0];
						node->input[1] = node->input[1]->input[0];
						rc = true;
					}
					break;
				case ADD:
					if (node->input[0]->zero()) {
						node->value = node->input[1]->value;
						node->name = node->input[1]->name;
						node->type = node->input[1]->type;
						node->input = node->input[1]->input;
						rc = true;
					} else if (node->input[1]->zero()) {
						node->value = node->input[0]->value;
						node->name = node->input[0]->name;
						node->type = node->input[0]->type;
						node->input = node->input[0]->input;
						rc = true;
					}
					break;
				case SUB:
					if (node->input[0]->zero()) {
						node->input[0] = node->input[1];
						node->input.resize(1);
						node->type = NEG;
						rc = true;
					} else if (node->input[1]->zero()) {
						node->value = node->input[0]->value;
						node->name = node->input[0]->name;
						node->type = node->input[0]->type;
						node->input = node->input[0]->input;
						rc = true;
					}
					break;
				default:
					break;
				}
			}
		}
	}

	std::vector<instruction_t> generate_instructions() {
		std::vector<instruction_t> code;
		std::set<std::string> free_vnames;
		std::unordered_map<std::string, std::weak_ptr<node>> used_vnames;
		int varcnt = 0;
		for (unsigned i = 0; i < inputs.size(); i++) {
			used_vnames[inputs[i]->name] = inputs[i];
		}
		inputs.resize(0);
		auto nodes = list();
		std::vector<int> done(nodes.size(), false);
		std::deque<int> last_reads;
		last_reads.push_back(0);
		auto next_node = [&nodes, &done, &last_reads]() {
			std::vector<int> candidates;
			for( unsigned i = 0; i < nodes.size(); i++) {
				if( !done[i]) {
					bool ready = true;
					for( auto j : node_ptr(nodes[i])->input) {
						if( !j->done) {
							ready = false;
							break;
						}
					}
					if( ready ) {
						candidates.push_back(i);
					}
				}
			}
			if( candidates.size() == 0 ) {
				return node_ptr(nullptr);
			} else {
				int besti = 0;
				int bestcnt = -1;
				for( unsigned i = 0; i < candidates.size(); i++) {
					int cnt = 0;
					for( auto j : node_ptr(nodes[candidates[i]])->input) {
						if( j.use_count() == 2) {
							cnt++;
						}
					}
					if( cnt > bestcnt) {
						bestcnt = cnt;
						besti = i;
					}
				}
				auto old = candidates;
				candidates.resize(0);
				for( unsigned i = 0; i < old.size(); i++) {
					int cnt = 0;
					for( auto j : node_ptr(nodes[old[i]])->input) {
						if( j.use_count() == 2) {
							cnt++;
						}
					}
					if( cnt == bestcnt) {
						candidates.push_back(old[i]);
					}
				}
				bestcnt = -1000000000;
				for( unsigned i = 0; i < old.size(); i++) {
					int cnt = 0;
					for( auto j : node_ptr(nodes[old[i]])->input) {
						int maxdist = 1000000000;
						if( *(j->name.c_str()) == 'x' ) {
							for( auto k : last_reads) {
								maxdist = std::min(maxdist, std::abs(k - atoi(j->name.c_str() + 2)));
							}
						}
						cnt -= maxdist;
					}
					if( cnt > bestcnt) {
						bestcnt = cnt;
						besti = i;
					}
				}
				done[candidates[besti]] = true;
				for( auto in : node_ptr(nodes[candidates[besti]])->input ) {
					if( *(in->name.c_str()) == 'x' ) {
						auto num = atoi(in->name.c_str() + 2);
						last_reads.push_front(num);
						while(last_reads.size() > 5 ) {
							last_reads.pop_back();
						}
					}
				}
				return node_ptr(nodes[candidates[besti]]);
			}
		};

		const auto genvarname = [&free_vnames, &varcnt, &used_vnames](std::weak_ptr<node> nd) {
			std::string nm;
			if (free_vnames.empty()) {
				varcnt++;
				nm = std::string("r") + std::to_string(varcnt);
			} else {
				nm = *(free_vnames.begin());
				free_vnames.erase(free_vnames.begin());
			}
			used_vnames[nm] = nd;
			return nm;
		};

		node_ptr n;
		while ((n = next_node()) != nullptr) {

			if (n->type == CON) {
				n->name = std::to_string(n->value);
			} else if (n->type == OUT) {
				if (used_vnames.find(n->name) != used_vnames.end()) {
					auto other = node_ptr(used_vnames[n->name]);
					other->name = genvarname(other);
					instruction_t i;
					i.op = ASN;
					i.vars.push_back(other->name);
					i.vars.push_back(n->name);
					code.push_back(i);
				} else {
					free_vnames.erase(n->name);
				}
				used_vnames[n->name] = n;
			} else if (n->type != IN) {
				n->name = genvarname(n);
			}
			switch (n->type) {
			case ADD:
			case SUB:
			case FMA:
			case MUL:
			case NEG:
			case OUT: {
				instruction_t i;
				i.op = n->type;
				i.vars.push_back(n->name);
				for (auto in : n->input) {
					i.vars.push_back(in->name);
				}
				for (auto& i : n->input) {
					if (i.use_count() == 1) {
						if (i->type != CON && i->type != OUT) {
							free_vnames.insert(i->name);
							used_vnames.erase(i->name);
						}
					}
				}
				code.push_back(i);
			}
				break;
			default:
				break;
			}
			n->done = true;
			n->input.resize(0);
		}
		return code;
	}
}
;

std::vector<node_ptr> fft_radix2(std::vector<node_ptr> xin, int N);
std::vector<node_ptr> fft_radix4(std::vector<node_ptr> xin, int N);

std::vector<node_ptr> fft_singleton(std::vector<node_ptr> xin, int N) {
	std::vector<node_ptr> xout(2 * N);
	std::vector<node_ptr> txp((N - 1) / 2 + 1);
	std::vector<node_ptr> txm((N - 1) / 2 + 1);
	std::vector<node_ptr> typ((N - 1) / 2 + 1);
	std::vector<node_ptr> tym((N - 1) / 2 + 1);
	std::vector<node_ptr> ap((N - 1) / 2 + 1);
	std::vector<node_ptr> am((N - 1) / 2 + 1);
	std::vector<node_ptr> bp((N - 1) / 2 + 1);
	std::vector<node_ptr> bm((N - 1) / 2 + 1);
	for (int j = 1; j <= (N - 1) / 2; j++) {
		txp[j] = xin[2 * j] + xin[2 * (N - j)];
		txm[j] = xin[2 * j] - xin[2 * (N - j)];
		typ[j] = xin[2 * j + 1] + xin[2 * (N - j) + 1];
		tym[j] = xin[2 * j + 1] - xin[2 * (N - j) + 1];
	}
	xout[0] = xin[0];
	xout[1] = xin[1];
	for (int j = 1; j <= (N - 1) / 2; j++) {
		xout[0] = xout[0] + txp[j];
		xout[1] = xout[1] + typ[j];
	}
	for (int j = 1; j <= (N - 1) / 2; j++) {
		ap[j] = xin[0];
		bp[j] = xin[1];
		for (int k = 1; k <= (N - 1) / 2; k++) {
			ap[j] = ap[j] + txp[k] * constant(cos(2.0 * M_PI * j * k / N));
			bp[j] = bp[j] + typ[k] * constant(cos(2.0 * M_PI * j * k / N));
			if (k == 1) {
				am[j] = tym[k] * constant(sin(2.0 * M_PI * j * k / N));
				bm[j] = txm[k] * constant(sin(2.0 * M_PI * j * k / N));
			} else {
				am[j] = am[j] + tym[k] * constant(sin(2.0 * M_PI * j * k / N));
				bm[j] = bm[j] + txm[k] * constant(sin(2.0 * M_PI * j * k / N));

			}
		}
		am[j] = -am[j];
		bm[j] = -bm[j];
		xout[2 * j] = ap[j] - am[j];
		xout[2 * j + 1] = bp[j] + bm[j];
		xout[2 * (N - j)] = ap[j] + am[j];
		xout[2 * (N - j) + 1] = bp[j] - bm[j];
	}
	return xout;
}

std::vector<node_ptr> fft_radix(std::vector<node_ptr> xin, int N) {
	if (N == 2) {
		return fft_radix4(xin, 2);
	} else if (N == 4) {
		return fft_radix4(xin, 4);
	} else if (N == 8) {
		return fft_radix4(xin, 8);
	} else if (N == 16) {
		return fft_radix4(xin, 16);
	} else {
		return fft_singleton(xin, N);
	}
}

std::vector<node_ptr> fft_short(std::vector<node_ptr> xin, int N) {
	if (N == 2) {
		return fft_radix4(xin, 2);
	} else if (N == 4) {
		return fft_radix4(xin, 4);
	} else if (N == 8) {
		return fft_radix4(xin, 8);
	} else {
		return fft_singleton(xin, N);
	}
}

std::vector<node_ptr> fft_prime_factor(std::vector<node_ptr> xin, int N1, int N2) {
	int N = N1 * N2;
	std::vector<node_ptr> xout(2 * N);
	std::vector<std::vector<node_ptr>> z(N2, std::vector<node_ptr>(2 * N1));
	std::vector<std::vector<node_ptr>> y(N1, std::vector<node_ptr>(2 * N2));
	for (int n1 = 0; n1 < N1; n1++) {
		for (int n2 = 0; n2 < N2; n2++) {
			y[n1][2 * n2] = xin[2 * ((N1 * n2 + N2 * n1) % N)];
			y[n1][2 * n2 + 1] = xin[2 * ((N1 * n2 + N2 * n1) % N) + 1];
		}
	}
	for (int n1 = 0; n1 < N1; n1++) {
		y[n1] = fft_radix(y[n1], N2);
	}
	for (int n1 = 0; n1 < N1; n1++) {
		for (int k2 = 0; k2 < N2; k2++) {
			z[k2][2 * n1] = y[n1][2 * k2];
			z[k2][2 * n1 + 1] = y[n1][2 * k2 + 1];
		}
	}
	for (int n2 = 0; n2 < N2; n2++) {
		z[n2] = fft_radix(z[n2], N1);
	}
	for (int n = 0; n < N; n++) {
		int n1 = n % N1;
		int n2 = n % N2;
		xout[2 * n] = z[n2][2 * n1];
		xout[2 * n + 1] = z[n2][2 * n1 + 1];
	}
	fprintf(stderr, "%i %i\n", xin.size(), xout.size());
	return xout;
}

std::vector<node_ptr> fft_radix4(std::vector<node_ptr> xin, int N) {
	if (N == 1) {
		return xin;
	} else if (N == 2) {
		std::vector<node_ptr> xout(4);
		auto x0 = xin[0] + xin[2];
		auto x1 = xin[0] - xin[2];
		auto y0 = xin[1] + xin[3];
		auto y1 = xin[1] - xin[3];
		xout[0] = x0;
		xout[1] = y0;
		xout[2] = x1;
		xout[3] = y1;
		return xout;
	}
	std::vector<node_ptr> xout(2 * N);
	std::vector<node_ptr> even, odd1, odd3;
	for (int n = 0; n < N / 2; n++) {
		even.push_back(xin[4 * n]);
		even.push_back(xin[4 * n + 1]);
	}
	for (int n = 0; n < N / 4; n++) {
		odd1.push_back(xin[8 * n + 2]);
		odd1.push_back(xin[8 * n + 3]);
		odd3.push_back(xin[8 * n + 6]);
		odd3.push_back(xin[8 * n + 7]);
	}
	if (N == 4) {
		even = fft_radix2(even, 2);
		odd1 = fft_radix2(odd1, 1);
		odd3 = fft_radix2(odd3, 1);
	} else if (N == 8) {
		even = fft_radix4(even, 4);
		odd1 = fft_radix2(odd1, 2);
		odd3 = fft_radix2(odd3, 2);
	} else {
		even = fft_radix4(even, N / 2);
		odd1 = fft_radix4(odd1, N / 4);
		odd3 = fft_radix4(odd3, N / 4);
	}
	const auto tw_mult = [N](int k, node_ptr r, node_ptr i) {
		std::pair<node_ptr, node_ptr> rc;
		double theta = -2.0 * M_PI * k / N;

		auto twr = constant(cos(theta));
		auto twi = constant(sin(theta));
		rc.first = r * twr - i * twi;
		rc.second = i * twr + r * twi;
		return rc;
	};
	for (int k = 0; k < N / 4; k++) {
		auto odds1 = tw_mult(k, odd1[2 * k], odd1[2 * k + 1]);
		auto odds3 = tw_mult(3 * k, odd3[2 * k], odd3[2 * k + 1]);
		auto zsr = odds1.first + odds3.first;
		auto zsi = odds1.second + odds3.second;
		auto zdr = odds1.first - odds3.first;
		auto zdi = odds1.second - odds3.second;
		auto ur0 = even[2 * k + 0] + zsr;
		auto ui0 = even[2 * k + 1] + zsi;
		auto ur1 = even[2 * (k + N / 4) + 0] + zdi;
		auto ui1 = even[2 * (k + N / 4) + 1] - zdr;
		auto ur2 = even[2 * k + 0] - zsr;
		auto ui2 = even[2 * k + 1] - zsi;
		auto ur3 = even[2 * (k + N / 4) + 0] - zdi;
		auto ui3 = even[2 * (k + N / 4) + 1] + zdr;
		xout[2 * k] = ur0;
		xout[2 * k + 1] = ui0;
		xout[2 * (k + N / 4)] = ur1;
		xout[2 * (k + N / 4) + 1] = ui1;
		xout[2 * (k + N / 2)] = ur2;
		xout[2 * (k + N / 2) + 1] = ui2;
		xout[2 * (k + 3 * N / 4)] = ur3;
		xout[2 * (k + 3 * N / 4) + 1] = ui3;
	}
	return xout;
}

std::vector<node_ptr> fft_radix2(std::vector<node_ptr> xin, int N) {
	if (N == 1) {
		return xin;
	}
	std::vector<node_ptr> xout(2 * N);
	std::vector<node_ptr> even, odd;
	for (int n = 0; n < N / 2; n++) {
		even.push_back(xin[4 * n]);
		even.push_back(xin[4 * n + 1]);
	}
	for (int n = 0; n < N / 2; n++) {
		odd.push_back(xin[4 * n + 2]);
		odd.push_back(xin[4 * n + 3]);
	}
	even = fft_radix2(even, N / 2);
	odd = fft_radix2(odd, N / 2);
	for (int k = 0; k < N / 2; k++) {
		double theta = -2.0 * M_PI * k / N;
		auto twr = constant(cos(theta));
		auto twi = constant(sin(theta));
		auto tr = odd[2 * k] * twr - odd[2 * k + 1] * twi;
		auto ti = odd[2 * k] * twi + odd[2 * k + 1] * twr;
		xout[2 * k] = even[2 * k] + tr;
		xout[2 * (k + N / 2)] = even[2 * k] - tr;
		xout[2 * k + 1] = even[2 * k + 1] + ti;
		xout[2 * (k + N / 2) + 1] = even[2 * k + 1] - ti;
	}
	return xout;
}

std::vector<node_ptr> fft_radixR(std::vector<node_ptr> xin, int N, int R) {
	if (N == 1) {
		return xin;
	} else if (N == R) {
		return fft_singleton(xin, R);
	}
	std::vector<node_ptr> xout(2 * N);
	std::vector<std::vector<node_ptr>> sub(R, std::vector<node_ptr>(2 * N / R));

	for (int n = 0; n < N / R; n++) {
		for (int r = 0; r < R; r++) {
			sub[r][2 * n] = xin[2 * (n * R + r)];
			sub[r][2 * n + 1] = xin[2 * (n * R + r) + 1];
		}
	}
	for (int r = 0; r < R; r++) {
		sub[r] = fft_radixR(sub[r], N / R, R);
	}
	for (int k = 1; k < N / R; k++) {
		for (int r = 1; r < R; r++) {
			auto x = sub[r][2 * k];
			auto y = sub[r][2 * k + 1];
			auto phi = -2.0 * M_PI * r * k / N;
			auto cs = constant(cos(phi));
			auto sn = constant(sin(phi));
			sub[r][2 * k + 0] = x * cs - y * sn;
			sub[r][2 * k + 1] = x * sn + y * cs;
		}
	}
	for (int k = 0; k < N / R; k++) {
		std::vector<node_ptr> y(2 * R);
		for (int r = 0; r < R; r++) {
			y[2 * r] = sub[r][2 * k];
			y[2 * r + 1] = sub[r][2 * k + 1];
		}
		y = fft_singleton(y, R);
		for (int r = 0; r < R; r++) {
			xout[2 * (r * N / R + k)] = y[2 * r];
			xout[2 * (r * N / R + k) + 1] = y[2 * r + 1];
		}
	}
	return xout;
}

void print_test_header() {
	printf("\n"
			"\n"
			"\n"
			"#include <fftw3.h>\n"
			"#include <cmath>\n"
			"#include <vector>\n"
			"#include <complex>\n"
			"#include <unordered_map>\n"
			"\t");
}

void print_code(const std::vector<instruction_t>& ins, std::string fname, int N) {
	printf("void %s(double* x) {;\n", fname.c_str());
	print_instructions(ins);
	printf("}\n");
}

void print_test_code(int N) {
	printf("\n"
			"\n"
			"void fftw(std::vector<std::complex<double>>& x) {\n"
			"\tconst int N = x.size();\n"
			"\tstatic std::unordered_map<int, fftw_plan> plans;\n"
			"\tstatic std::unordered_map<int, fftw_complex*> in;\n"
			"\tstatic std::unordered_map<int, fftw_complex*> out;\n"
			"\tif (plans.find(N) == plans.end()) {\n"
			"\t\tin[N] = (fftw_complex*) malloc(sizeof(fftw_complex) * N);\n"
			"\t\tout[N] = (fftw_complex*) malloc(sizeof(fftw_complex) * N);\n"
			"\t\tplans[N] = fftw_plan_dft_1d(N, in[N], out[N], FFTW_FORWARD, FFTW_ESTIMATE);\n"
			"\t}\n"
			"\tauto* i = in[N];\n"
			"\tauto* o = out[N];\n"
			"\tfor (int n = 0; n < N; n++) {\n"
			"\t\ti[n][0] = x[n].real();\n"
			"\t\ti[n][1] = x[n].imag();\n"
			"\t}\n"
			"\tfftw_execute(plans[N]);\n"
			"\tfor (int n = 0; n < N; n++) {\n"
			"\t\tx[n].real(o[n][0]);\n"
			"\tx[n].imag(o[n][1]);\n"
			"\t}\n"
			"}\n"
			"\n"
			"\n"
			"double rand1() {\n"
			"\treturn (rand() + 0.5) / RAND_MAX;\n"
			"}\n"
			"\n");
	printf("#include <chrono>\n"
			"class timer {\n"
			"\tstd::chrono::time_point<std::chrono::high_resolution_clock> start_time;\n"
			"\tdouble time;\n"
			"public:\n"
			"\tinline timer() {\n"
			"\t\ttime = 0.0;\n"
			"\t}\n"
			"\tinline void stop() {\n"
			"\t\tstd::chrono::time_point<std::chrono::high_resolution_clock> stop_time = std::chrono::high_resolution_clock::now();\n"
			"\t\tstd::chrono::duration<double> dur = stop_time - start_time;\n"
			"\t\ttime += dur.count();\n"
			"\t}\n"
			"\tinline void start() {\n"
			"\t\tstart_time = std::chrono::high_resolution_clock::now();\n"
			"\t}\n"
			"\tinline void reset() {\n"
			"\t\ttime = 0.0;\n"
			"\t}\n"
			"\tinline double read() {\n"
			"\t\treturn time;\n"
			"\t}\n"
			"};\n"
			"\n"
			"\n"
			"\n"
			"int main() {\n"
			"\tconstexpr int N = %i;\n"
			"\tstd::vector<double> xin(2 * N);\n"
			"\tstd::vector<std::complex<double>> y(N);\n"
			"\ttimer tm1, tm2;\n"
			"\tfor( int i = 0; i < 256; i++) {\n"
			"\t\tfor( int n = 0; n < 2 * N; n++) {\n"
			"\t\t\txin[n] = rand1();\n"
			"\t\t\tif( n %% 2 == 0 ) {\n"
			"\t\t\t\ty[n / 2].real(xin[n]);\n"
			"\t\t\t} else {\n"
			"\t\t\t\ty[n / 2].imag(xin[n]);\n"
			"\t\t\t}\n"
			"\t\t}\n"
			"\t\ttm1.start();\n"
			"\t\ttest(xin.data());\n"
			"\t\tauto xout = xin;\n"
			"\t\ttm1.stop();\n"
			"\t\ttm2.start();\n"
			"\t\tfftw(y);\n"
			"\t\ttm2.stop();\n"
			"\t\tdouble error = 0.0;\n"
			"\t\tfor( int n = 0; n < N; n++) {\n"
			"\t\t\terror += std::pow(xout[2 * n] - y[n].real(), 2);\n"
			"\t\t\terror += std::pow(xout[2 * n + 1] - y[n].imag(), 2);\n"
			"\t\t\t//printf( \"%%i %%e %%e %%e %%e;\\n\", n, xout[2*n], xout[2*n+1], y[n].real(), y[n].imag())\n;\n"
			"\t\t}\n"
			"\t\terror = error / (2.0 * N);\n"
			"\t\tif( i == 255 ) {\n"
			"\t\t\tprintf( \"Error = %%e\\n\", error );\n"
			"\t\t}\n"
			"\t}\n"
			"\tprintf( \"%%e %%e %%e\\n\", tm1.read(), tm2.read(), tm2.read() / tm1.read() );\n"
			"\t\n"
			"}\n"
			"", N);
}

int main(int argc, char **argv) {
//	constexpr int N1 = 7;
//	constexpr int N2 = 16;
	constexpr int N = 9;
//	constexpr int N = N1 * N2;
	dag graph;
	graph.set_outputs(fft_radixR(graph.get_inputs(2 * N), N, 3));
//	graph.set_outputs(fft_prime_factor(graph.get_inputs(2 * N1 * N2), N1, N2));
	//graph.set_outputs(fft_radix4(graph.get_inputs(2 * N), N));
//	graph.set_outputs(fft_singleton(graph.get_inputs(2 * N), N));
	graph.optimize_constants();
	graph.propagate_signs();
	graph.optimize_common_subexps();
//	graph.optimize_constants();
	auto ins = graph.generate_instructions();
	print_test_header();
	print_code(ins, "test", 2 * N);
	print_test_code(N);
	int cnt = 0;
	int acnt = 0;
	int add = 0;
	int mul = 0;
	int fma = 0;
	int neg = 0;
	for (auto i : ins) {
		switch (i.op) {
		case FMA:
			fma++;
			break;
		case ADD:
		case SUB:
			add++;
			break;
		case MUL:
			mul++;
			break;
		case NEG:
			neg++;
			break;
		default:
			acnt++;
			break;
		}
	}
	cnt = neg + add + mul + fma;
	fprintf(stderr, "op cnt = %i = %i add/subs + %i muls + %i fmas + %i negations + %i assignments\n", cnt, add, mul, fma, neg, acnt);
	return 0;
}
