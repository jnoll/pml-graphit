process test_control_flow {
    iteration Iteration {
	action i_one {
	    agent { Foo }
	    requires { r1 }
	    provides { r2 }
	}
	action i_two {
	    agent { Bar }
	    requires { r2 }
	    provides { r3 }
	}
    }
    branch Branch {
	action b_one {
	    agent { Foo && Bar}
	    requires { r3 && r4 }
	    provides { r5 && r4 }
	}
	action b_two {
	    agent { Bar && Baz}
	}
    }
    selection Selection {
	action s_one {
	    agent { Baz && Bar}
	}
	action s_two {
	    agent { Baz || Foo}
	}
	action s_three {
	    agent { Baz || Foo}
	}
	action s_four {
	    agent { Baz || Foo}
	}
    }
    task Task_chain {
	action tc_one {
	    agent { Baz && Bar}
	}
	action tc_two {
	    agent { Baz || Foo}
	}
    }
    sequence Nested_0_has_three_levels_of_children_branch_selection_action {
	action nested_action_at_level_1 {}
	branch Nested_Branch_at_level_1 {
	    selection Nested_Selection_at_level_2 {
		action nested_action_at_level_3_act {}
		action nested_action_2_at_level_3_act {}
	    }
	    action nested_action_at_level_2 {}
	}
    }
    sequence Sub_process {
	action normal {}
	action sub_process subprocess {}
	action normal {}
    }
}
