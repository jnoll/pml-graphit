## jhnoll@gmail.com

build:
	stack build

test:	test-select

test-happy:
	stack exec pml-graphit -- test_control_flow.pml
test-textwidth:
	stack exec pml-graphit -- --width 20 test_control_flow.pml
test-select:
	stack exec pml-graphit -- --subtree b_one test_control_flow.pml
test-select2:
	stack exec pml-graphit -- --subtree Branch test_control_flow.pml
test-select2swim:
	stack exec pml-graphit -- --subtree Branch --swim test_control_flow.pml
test-select3:
	stack exec pml-graphit -- --subtree test_control_flow  test_control_flow.pml
test-select3swim:
	stack exec pml-graphit -- --subtree test_control_flow --swim test_control_flow.pml
test-depth1:
	stack exec pml-graphit -- --depth 1 test_control_flow.pml
test-depth2:
	stack exec pml-graphit -- --depth 2 test_control_flow.pml
test-depth3:
	stack exec pml-graphit -- --depth 3 test_control_flow.pml
test-depthswim:
	stack exec pml-graphit -- --depth 2 --swim test_control_flow.pml
