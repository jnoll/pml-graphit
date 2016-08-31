## jhnoll@gmail.com

build:
	stack build
install:
	stack install

test:	test-happy test-happyswim test-swim test-textwidth test-select test-select2  test-select2swim test-select3 test-select3swim test-depth1 test-depth2 test-depth3 test-depthswim test-agents

test-happy:
	@echo "happy path"
	@stack exec pml-graphit -- test_control_flow.pml > test.puml
	@plantuml test.puml
	@eog test.png
test-happyswim:
	@echo "happy swimlanes: Foo: 2, Bar: 1, Baz: 1"
	@stack exec pml-graphit -- --swim test_swimlanes.pml > test.puml
	@plantuml test.puml
	@eog test.png
test-swim:
	@echo "swimlanes2: Foo: 4, Bar noted: 3, Baz noted: 3"
	@stack exec pml-graphit -- --swim test_swimlanes2.pml > test.puml
	@plantuml test.puml
	@eog test.png
test-textwidth:
	@echo "test_control_flow.pml + textwidth = 20"
	@stack exec pml-graphit -- --width 20 test_control_flow.pml > test.puml
	@plantuml test.puml
	@eog test.png
test-select:
	@echo "test_control_flow.pml + subtree b_one"
	@stack exec pml-graphit -- --subtree b_one test_control_flow.pml > test.puml
	@plantuml test.puml
	@eog test.png
test-select2:
	@echo "test_control_flow.pml + subtree Branch"
	@stack exec pml-graphit -- --subtree Branch test_control_flow.pml > test.puml
	@plantuml test.puml
	@eog test.png
test-select2swim:
	@echo "test_control_flow.pml + subtree Branch + swimlanes: Foo 1, Bar 1, Baz noted"
	@stack exec pml-graphit -- --subtree Branch --swim test_control_flow.pml > test.puml
	@plantuml test.puml
	@eog test.png
test-select3:
	@echo "test_control_flow.pml + subtree root"
	@stack exec pml-graphit -- --subtree test_control_flow  test_control_flow.pml > test.puml
	@plantuml test.puml
	@eog test.png
test-select3swim:
	@echo "test_control_flow.pml + subtree root + swimlanes"
	@stack exec pml-graphit -- --subtree test_control_flow --swim test_control_flow.pml > test.puml
	@plantuml test.puml
	@eog test.png
test-depth1:
	@echo "test_control_flow.pml + pruned at depth 1"
	@stack exec pml-graphit -- --depth 1 test_control_flow.pml > test.puml
	@plantuml test.puml
	@eog test.png
test-depth2:
	@echo "test_control_flow.pml + pruned at depth 2"
	@stack exec pml-graphit -- --depth 2 test_control_flow.pml > test.puml
	@plantuml test.puml
	@eog test.png
test-depth3:
	@echo "test_control_flow.pml + pruned at depth 3"
	@stack exec pml-graphit -- --depth 3 test_control_flow.pml > test.puml
	@plantuml test.puml
	@eog test.png
test-depthswim:
	@echo "test_control_flow.pml + pruned at depth 2 + swimlanes"
	@stack exec pml-graphit -- --depth 2 --swim test_control_flow.pml > test.puml
	@plantuml test.puml
	@eog test.png
test-agents:
	@echo "test_control_flow.pml + agents: Bar, Baz, Foo"
	@stack exec pml-graphit -- --agents test_control_flow.pml 

test-requires:
	@echo "test_control_flow.pml + requires: "
	@stack exec pml-graphit -- --requires test_control_flow.pml 

test-provides:
	@echo "test_control_flow.pml + provides: "
	@stack exec pml-graphit -- --provides test_control_flow.pml 

