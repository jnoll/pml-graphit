process swimlanes {
    action a_foo_bar_baz {
	agent { Foo && Bar || Baz }
    }
    action a_foo {
	agent { Foo }
    }
    action a_bar {
	agent { Bar }
    }
    action a_baz {
	agent { Baz }
    }
}
