#include <memory>
#include <vector>
#include <complex>
#include <unordered_set>
#include <stack>
#include <set>
#include <cassert>
#include <unordered_map>

class dag;
class node;

using node_ptr = std::shared_ptr<node>;

#define tiny 1e-14

enum node_type {
	ADD, SUB, MUL, NEG, IN, CON, OUT, ASN
};

struct instruction_t {
	node_type op;
	std::vector<std::string> vars;
};

void print_instructions(const std::vector<instruction_t>& ins) {
	std::set<std::string> declared;
	for (int i = 0; i < ins.size(); i++) {
		const auto var = ins[i].vars[0];
		if (declared.find(var) == declared.end()) {
			declared.insert(var);
			if (var[0] == 'r') {
				printf("\tdouble %s;\n", var.c_str());
			}
		}
		printf("\t%s = ", var.c_str());
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
		};
		if (ins[i].op != NEG && ins[i].op != OUT&& ins[i].op !=ASN) {
			printf("%s;\n", ins[i].vars[2].c_str());
		} else {
			printf(";\n");
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

	bool equivalent(node_ptr other, bool term = false) {
		bool rc;
		rc = false;
		for (int i = 0; i < 2; i++) {
			if (type == CON) {
			} else {
				if (type == other->type) {
					rc = true;
					for (int i = 0; i < input.size(); i++) {
						if (input[i]->type == CON) {
							if (other->input[i]->type == CON) {
								if (std::abs(
										other->input[i]->value
												- input[i]->value) > tiny) {
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
	friend node_ptr create_input(int i);
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
		for (int i = 0; i < outputs.size(); i++) {
			auto tmp = outputs[i]->list(touched);
			nodes.insert(nodes.end(), tmp.begin(), tmp.end());
		}
		return nodes;
	}
	std::vector<node_ptr>& get_inputs(int N) {
		for (int i = 0; i < 2 * N; i++) {
			inputs.push_back(create_input(i));
		}
		return inputs;
	}
	void set_outputs(std::vector<node_ptr>&& outs) {
		outputs = std::move(outs);
		for (int i = 0; i < outputs.size(); i++) {
			outputs[i] = node::create(OUT, outputs[i]);
			outputs[i]->name = std::string("x[") + std::to_string(i)
					+ std::string("]");
		}
	}

	void optimize() {
		auto nodes = list();
		std::unordered_map<node*, node_ptr> node_map;
		for (auto wnode : nodes) {
			if (wnode.use_count() == 0) {
				continue;
			}

			auto node = node_ptr(wnode);
			node_ptr ptr = nullptr;
			if (node->type == ADD || node->type == SUB || node->type == MUL
					|| node->type == NEG) {
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
				if (in->type == ADD || in->type == SUB || in->type == MUL
						|| in->type == NEG) {
					in = node_map[in.get()];
				}
			}
			switch (node->type) {
			case MUL:
				if (node->input[0]->zero() || node->input[1]->zero()) {
					node->value = 0.0;
					node->name = std::to_string(0);
					node->type = CON;
					node->input.resize(0);
				} else if (node->input[0]->one()) {
					node->value = node->input[1]->value;
					node->name = node->input[1]->name;
					node->type = node->input[1]->type;
					node->input = node->input[1]->input;
				} else if (node->input[1]->one()) {
					node->value = node->input[0]->value;
					node->name = node->input[0]->name;
					node->type = node->input[0]->type;
					node->input = node->input[0]->input;
				} else if (node->input[0]->none()) {
					node->input[0] = node->input[1];
					node->input.resize(1);
					node->type = NEG;
				} else if (node->input[1]->none()) {
					node->input.resize(1);
					node->type = NEG;
				} else if (node->input[0]->type == NEG
						&& node->input[1]->type == NEG) {
					node->input[0] = node->input[0]->input[0];
					node->input[1] = node->input[1]->input[0];
				} else if (node->input[0]->type == NEG) {
					auto tmp = *node;
					node->input[0] = node->input[0]->input[1] * node->input[1];
					node->input[0]->done = tmp.done;
					node->input[0]->name = tmp.name;
					node->input.resize(1);
					node->type = NEG;
				} else if (node->input[1]->type == NEG) {
					auto tmp = *node;
					node->input[0] = node->input[1]->input[0] * node->input[0];
					node->input[0]->done = tmp.done;
					node->input[0]->name = tmp.name;
					node->input.resize(1);
					node->type = NEG;
				}
				break;
			case ADD:
				if (node->input[0]->zero()) {
					node->value = node->input[1]->value;
					node->name = node->input[1]->name;
					node->type = node->input[1]->type;
					node->input = node->input[1]->input;
				} else if (node->input[1]->zero()) {
					node->value = node->input[0]->value;
					node->name = node->input[0]->name;
					node->type = node->input[0]->type;
					node->input = node->input[0]->input;
				} else if (node->input[0]->type == NEG) {
					node->type = SUB;
					node->input[0] = node->input[0]->input[0];
					std::swap(node->input[0], node->input[1]);
				} else if (node->input[1]->type == NEG) {
					node->type = SUB;
					node->input[1] = node->input[1]->input[0];
				} else if (node->input[0]->type == NEG
						&& node->input[1]->type == NEG) {
					auto new_node = node->input[0]->input[0]
							+ node->input[1]->input[0];
					node->type = NEG;
					node->input[0] = new_node;
					node->input.resize(1);
				}
				break;
			case SUB:
				if (node->input[0]->zero()) {
					node->input[0] = node->input[1];
					node->input.resize(1);
					node->type = NEG;
				} else if (node->input[1]->zero()) {
					node->value = node->input[0]->value;
					node->name = node->input[0]->name;
					node->type = node->input[0]->type;
					node->input = node->input[0]->input;
				} else if (node->input[0]->type == NEG) {
					auto tmp = *node;
					node->input[0] = node->input[0]->input[0] + node->input[1];
					node->input[0]->done = tmp.done;
					node->input[0]->name = tmp.name;
					node->input.resize(1);
					node->type = NEG;
				} else if (node->input[1]->type == NEG) {
					node->type = ADD;
					node->input[1] = node->input[1]->input[0];
				} else if (node->input[0]->type == NEG
						&& node->input[1]->type == NEG) {
					node->type = ADD;
					node->input[0] = node->input[0]->input[0];
					node->input[1] = node->input[1]->input[0];
					std::swap(node->input[0], node->input[1]);
				}
				break;
			}
		}
	}

	std::vector<instruction_t> generate_instructions() {
		std::vector<instruction_t> code;
		std::stack<std::string> free_vnames;
		std::unordered_map<std::string, std::weak_ptr<node>> used_vnames;
		int varcnt = 0;
		for (int i = 0; i < inputs.size(); i++) {
			used_vnames[inputs[i]->name] = inputs[i];
		}
		inputs.resize(0);
		auto nodes = list();
		const auto genvarname =
				[&free_vnames, &varcnt, &used_vnames](std::weak_ptr<node> nd) {
					std::string nm;
					if (free_vnames.empty()) {
						varcnt++;
						nm = std::string("r") + std::to_string(varcnt);
					} else {
						nm = free_vnames.top();
						free_vnames.pop();
					}
					used_vnames[nm] = nd;
					return nm;
				};
		for (auto wnode : nodes) {
			if (wnode.use_count() == 0) {
				continue;
			}
			auto n = node_ptr(wnode);
			if (n->type == CON) {
				n->name = std::to_string(n->value);
			} else if( n->type == OUT) {
				if( used_vnames.find(n->name) != used_vnames.end()) {
					auto other = node_ptr(used_vnames[n->name]);
					other->name = genvarname(other);
					instruction_t i;
					i.op = ASN;
					i.vars.push_back(other->name);
					i.vars.push_back(n->name);
					code.push_back(i);

				}
			} else if (n->type != IN) {
				n->name = genvarname(n);
			}
			switch (n->type) {
			case ADD:
			case SUB:
			case MUL:
			case NEG:
			case OUT:
				instruction_t i;
				i.op = n->type;
				i.vars.push_back(n->name);
				for (auto in : n->input) {
					i.vars.push_back(in->name);
				}
				n->done = true;
				for (auto& i : n->input) {
					if (i.use_count() == 1) {
						if (i->type != CON) {
							free_vnames.push(i->name);
							used_vnames.erase(i->name);
						}
					}
				}
				code.push_back(i);
				break;
			}
			n->input.resize(0);
		}
		return code;
	}
}
;

std::vector<node_ptr> fft_radix2(std::vector<node_ptr> xin, int N);

std::vector<node_ptr> fft_radix4(std::vector<node_ptr> xin, int N) {
	if (N == 1) {
		return xin;
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

void print_code(const std::vector<instruction_t>& ins, std::string fname,
		int N) {
	printf("void %s(double* x) {;\n", fname.c_str());
	print_instructions(ins);
	printf("}\n");
}

void print_test_code(int N) {
	printf(
			"\n"
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
	printf(
			"#include <chrono>\n"
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
	constexpr int N = 16;
	dag graph;
	graph.set_outputs(fft_radix4(graph.get_inputs(2 * N), N));
	graph.optimize();
	graph.optimize();
	graph.optimize();
	auto ins = graph.generate_instructions();
	print_test_header();
	print_code(ins, "test", 2 * N);
	print_test_code(N);
	fprintf(stderr, "op cnt = %i\n", ins.size());
	return 0;
}
