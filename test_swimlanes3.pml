process swimlanes {
    action a_foo_bar_or_baz {
	agent { Foo && Bar || Baz }
    }
}
