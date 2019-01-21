process swimlanes_and_or {
    action a_foo_bar_or_baz {
	agent { Foo && Bar || Baz }
    }
    action a_foo_bar {
	agent { Foo && Bar}
    }
    action a_foo_baz {
	agent { Foo && Baz }
    }
    action a_foo_baz_baz {
	agent { Foo && Bar && Baz }
    }
}
