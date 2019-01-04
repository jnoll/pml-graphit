process t {
    action a {
	requires { a_r }
	provides { a_s.anAttr }
    }
    action b {
	requires { b_s.anAttr }
	provides { b_s.anAttr == "string value" }
    }
    action b2 {
	requires { b2_s.anAttr >= 1}
	provides { b2_s.anAttr <= "string value 2" }
    }
    action c {
	requires { c_t.anAttr == s.anAttr }
	provides { c_t.anAttr_lt < s.anAttr_gt }
    }
    action d {
	requires { d_t != s }
	provides { d_t == s }
    }
    action e {
	requires { e_u || v }
	provides { e_v && r }
    }
}
