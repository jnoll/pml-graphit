process t {
    action a {
	requires { r }
	provides { s.attr }
    }
    action b {
	requires { s.attr }
	provides { s.attr == "string value" }
    }
    action b2 {
	requires { s.attr }
	provides { s.attr <= "string value 2" }
    }
    action c {
	requires { t.attr == s.attr }
	provides { t.attr_lt < s.attr_gt }
    }
    action d {
	requires { t != s }
	provides { t == s }
    }
}
