package sn.esmt.applicationsuivistage.controllers;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

import jakarta.validation.Valid;
import sn.esmt.applicationsuivistage.entities.user.Encadreur;
import sn.esmt.applicationsuivistage.enums.ErrorType;
import sn.esmt.applicationsuivistage.exceptions.ResourceAlreadyExistsException;
import sn.esmt.applicationsuivistage.exceptions.ResourceNotFoundException;
import sn.esmt.applicationsuivistage.services.EncadreurService;

@Controller
@RequestMapping("/encadreur")
public class EncadreurController {

	@Autowired
	private EncadreurService encadreurService;

	private String prefix = "views/encadreur/";

	private static final Logger logger = LoggerFactory.getLogger(EncadreurController.class);

	@GetMapping
	public String index(@RequestParam(name = "page", defaultValue = "1") int page,
			@RequestParam(name = "size", defaultValue = "5") int size,
			@RequestParam(name = "keyword", required = false) String keyword, Model model) {
		try {

			Page<Encadreur> pageEncadreur;

			if (keyword == null) {
				pageEncadreur = encadreurService.list(page - 1, size);
				model.addAttribute("encadreurs", pageEncadreur.getContent());
			} else {
				pageEncadreur = encadreurService.listByEmailContaining(page - 1, size, keyword);
				model.addAttribute("encadreurs", pageEncadreur.getContent());
				model.addAttribute("keyword", keyword);
			}

			model.addAttribute("pages", pageEncadreur.getTotalPages());
			model.addAttribute("pageSize", size);
			model.addAttribute("currentPage", pageEncadreur.getNumber() + 1);
			model.addAttribute("activePage", "encadreur");

		} catch (Exception e) {
			logger.error("{}: {}", ErrorType.CHARGEMENT_ERROR.name(), e.getMessage());
			model.addAttribute("error", ErrorType.CHARGEMENT_ERROR.get());
		}

		return prefix + "index";

	}

	@GetMapping("add")
	//@PreAuthorize("hasRole('ROLE_ADMIN')")
	public String addForm(Model model) {
		model.addAttribute("encadreur", new Encadreur());
		model.addAttribute("activePage", "encadreur");

		return prefix + "add";
	}

	@PostMapping("save")
	public String save(@Valid @ModelAttribute("encadreur") Encadreur encadreur, BindingResult bindingResult,
			Model model, RedirectAttributes redirectAttributes) {

		if (bindingResult.hasErrors()) {
			model.addAttribute("activePage", "encadreur");
			return prefix + "add";
		}

		try {
			encadreurService.create(encadreur);
			redirectAttributes.addFlashAttribute("message", "L'encadreur a bien été enregistré");

		} catch (ResourceAlreadyExistsException e) {
			logger.error("{}: {}", ErrorType.CREATION_ERROR.name(), e.getMessage());
			model.addAttribute("error", e.getMessage());
			model.addAttribute("activePage", "encadreur");

			return prefix + "add";

		} catch (Exception e) {
			logger.error("{}: {}", ErrorType.CREATION_ERROR.name(), e.getMessage());
			redirectAttributes.addFlashAttribute("error", ErrorType.CREATION_ERROR.get());
		}

		return "redirect:/encadreur";

	}

	@GetMapping("edit/{id}")
	public String updateForm(@PathVariable("id") Long id, Model model, RedirectAttributes redirectAttributes) {

		try {
			model.addAttribute("encadreur", encadreurService.find(id));
			model.addAttribute("activePage", "encadreur");

		} catch (Exception e) {
			logger.error("{}: {}", ErrorType.CHARGEMENT_ERROR.name(), e.getMessage());
			redirectAttributes.addFlashAttribute("error", ErrorType.CHARGEMENT_ERROR.get());

			return "redirect:/encadreur";
		}

		return prefix + "update";
	}

	@PostMapping("update")
	public String update(@Valid @ModelAttribute("encadreur") Encadreur encadreur, BindingResult bindingResult,
			Model model, RedirectAttributes redirectAttributes) {
		if (bindingResult.hasErrors()) {
			model.addAttribute("activePage", "encadreur");
			return prefix + "update";
		}

		try {
			encadreurService.update(encadreur, encadreur.getId());
			redirectAttributes.addFlashAttribute("message", "L'encadreur a bien été modifié");

		} catch (ResourceNotFoundException | ResourceAlreadyExistsException e) {
			logger.error("{}: {}", ErrorType.MODIFICATION_ERROR.name(), e.getMessage());
			model.addAttribute("error", e.getMessage());
			model.addAttribute("activePage", "encadreur");

			return prefix + "update";

		} catch (Exception e) {
			logger.error("{}: {}", ErrorType.MODIFICATION_ERROR.name(), e.getMessage());
			redirectAttributes.addFlashAttribute("error", ErrorType.MODIFICATION_ERROR.get());
		}
		return "redirect:/encadreur";
	}

	@GetMapping("delete/{id}")
	public String delete(@PathVariable("id") Long id, RedirectAttributes redirectAttributes) {
		try {
			encadreurService.delete(id);
			redirectAttributes.addFlashAttribute("message", "L'encadreur " + id + " a bien été supprimé");

		} catch (ResourceNotFoundException e) {
			logger.error("{}: {}", ErrorType.SUPPRESSION_ERROR.name(), e.getMessage());
			redirectAttributes.addFlashAttribute("error", e.getMessage());

		} catch (Exception e) {
			logger.error("{}: {}", ErrorType.SUPPRESSION_ERROR.name(), e.getMessage());
			redirectAttributes.addFlashAttribute("error", ErrorType.SUPPRESSION_ERROR.get());
		}

		return "redirect:/encadreur";
	}

	@GetMapping("details/{id}")
	public String details(@PathVariable("id") Long id, Model model, RedirectAttributes redirectAttributes) {
		try {
			model.addAttribute("encadreur", encadreurService.find(id));
			model.addAttribute("activePage", "encadreur");

		} catch (Exception e) {
			logger.error("{}: {}", ErrorType.CHARGEMENT_ERROR.name(), e.getMessage());
			redirectAttributes.addFlashAttribute("error", ErrorType.CHARGEMENT_ERROR.get());

			return "redirect:/encadreur";

		}
		return prefix + "details";
	}

	@GetMapping("{id}/etudiants/remove/{idEtu}")
	public String removeStuduent(@PathVariable("id") Long id, @PathVariable("idEtu") Long idEtu,
			RedirectAttributes redirectAttributes) {

		try {
			encadreurService.removeStudent(id, idEtu);
			redirectAttributes.addFlashAttribute("message", "L'etudiant " + idEtu + " a bien été dissocié");

		} catch (ResourceNotFoundException | IllegalArgumentException e) {
			logger.error("{}: {}", ErrorType.SUPPRESSION_ERROR.name(), e.getMessage());
			redirectAttributes.addFlashAttribute("error", e.getMessage());

		} catch (Exception e) {
			logger.error("{}: {}", ErrorType.SUPPRESSION_ERROR.name(), e.getMessage());
			redirectAttributes.addFlashAttribute("error", ErrorType.SUPPRESSION_ERROR.get());
		}

		return "redirect:/encadreur/details/" + id;
	}

}
