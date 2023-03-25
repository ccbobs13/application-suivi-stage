package sn.esmt.applicationsuivistage.controllers;

import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;

/**
 * @author ccb
 *
 */
@Controller
public class LoginController {
	@GetMapping("/login")
	String login() {
		return "login";
	}
}