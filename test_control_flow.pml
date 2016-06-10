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

}
