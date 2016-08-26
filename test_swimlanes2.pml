process swimlanes {
    action a_foo_bar_baz {
	agent { Foo && Bar || Baz }
    }
    action a_foo {
	agent { Foo && Bar}
    }
    action a_bar {
	agent { Foo && Baz }
    }
    action a_baz {
	agent { Foo && Bar && Baz }
    }
}
