process test_control_flow {
    iteration Iteration {
	action i_one {
	    agent { Foo }
	}
	action i_two {
	    agent { Bar }
	}
    }
    branch Branch {
	action b_one {
	    agent { Foo }
	}
	action b_two {
	    agent { Bar }
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
}
