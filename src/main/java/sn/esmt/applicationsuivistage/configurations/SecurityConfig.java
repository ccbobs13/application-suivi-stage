package sn.esmt.applicationsuivistage.configurations;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.config.annotation.method.configuration.EnableMethodSecurity;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.security.web.util.matcher.AntPathRequestMatcher;

@Configuration
@EnableWebSecurity
@EnableMethodSecurity
public class SecurityConfig {
	
	
	@Bean
	SecurityFilterChain filterChain(HttpSecurity http) throws Exception {
		http
		.cors()
		.and()
		.authorizeHttpRequests(request -> {
			request.requestMatchers("/css/**", "/js/**", "/images/**" , "/webjars/bootstrap/**").permitAll();
			request.anyRequest().authenticated();
			})
		
		//Gestion du Login
		
		.formLogin(form -> form
				.loginPage("/login").permitAll()
				.defaultSuccessUrl("/", true))
		
		//Gestion du Logout

		.logout(out -> {
			out.logoutRequestMatcher(new AntPathRequestMatcher("/logout"));
			out.logoutSuccessUrl("/login?logout").permitAll();
		})
			
		//Gestion des erreurs 
		// => GERE AUTOMATIQUEMENT EN CREANT UN DOSSIER ERROR DANS TEMPLATES ET EN CREANT LES FICHIERS HTML AVEC LES CODE D'ERREUR
		// PUIS AJOUTER UN FICHIER ERROR DANS TEMPLATES POUR TOUTES LES AUTRES ERREURS
//		.exceptionHandling(error -> error.accessDeniedPage("/error"))
		;
	
		return http.build();
	}
}