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
import sn.esmt.applicationsuivistage.entities.user.Etudiant;
import sn.esmt.applicationsuivistage.enums.ErrorType;
import sn.esmt.applicationsuivistage.exceptions.ResourceAlreadyExistsException;
import sn.esmt.applicationsuivistage.exceptions.ResourceNotFoundException;
import sn.esmt.applicationsuivistage.services.EncadreurService;
import sn.esmt.applicationsuivistage.services.EtudiantService;
import sn.esmt.applicationsuivistage.services.StageService;

@Controller
@RequestMapping("/etudiant")
public class EtudiantController {

	@Autowired
	private EtudiantService etudiantService;

	@Autowired
	private EncadreurService encadreurService;

	@Autowired
	private StageService stageService;

	private String prefix = "views/etudiant/";

	private static final Logger logger = LoggerFactory.getLogger(EtudiantController.class);

	@GetMapping
	public String index(@RequestParam(name = "page", defaultValue = "1") int page,
			@RequestParam(name = "size", defaultValue = "5") int size,
			@RequestParam(name = "keyword", required = false) String keyword, Model model) {
		
		try {
			
			Page<Etudiant> pageEtudiant;

			if (keyword == null) {
				pageEtudiant = etudiantService.list(page - 1, size);
				model.addAttribute("etudiants", pageEtudiant.getContent());
			} else {
				pageEtudiant = etudiantService.listByEmailContaining(page - 1, size, keyword);
				model.addAttribute("etudiants", pageEtudiant.getContent());
				model.addAttribute("keyword", keyword);
			}

			model.addAttribute("pages", pageEtudiant.getTotalPages());
			model.addAttribute("pageSize", size);
			model.addAttribute("currentPage", pageEtudiant.getNumber() + 1);
			model.addAttribute("activePage", "etudiant");

		} catch (Exception e) {
			logger.error("{}: {}", ErrorType.CHARGEMENT_ERROR.name(), e.getMessage());
			model.addAttribute("error", ErrorType.CHARGEMENT_ERROR.get());
		}
		return prefix + "index";

	}

	@GetMapping("add")
	public String addForm(Model model, RedirectAttributes redirectAttributes) {
		try {
			model.addAttribute("etudiant", new Etudiant());
			model.addAttribute("encadreurs", encadreurService.list());
			model.addAttribute("stages", stageService.list());
			model.addAttribute("activePage", "etudiant");

		} catch (Exception e) {
			logger.error("{}: {}", ErrorType.CHARGEMENT_ERROR.name(), e.getMessage());
			redirectAttributes.addFlashAttribute("error", ErrorType.CHARGEMENT_ERROR.get());

			return "redirect:/etudiant";
		}
		return prefix + "add";

	}

	@PostMapping("save")
	public String save(@Valid @ModelAttribute("etudiant") Etudiant etudiant, BindingResult bindingResult, Model model,
			RedirectAttributes redirectAttributes) {

		if (bindingResult.hasErrors()) {
			model.addAttribute("encadreurs", encadreurService.list());
			model.addAttribute("stages", stageService.list());
			model.addAttribute("activePage", "etudiant");

			return prefix + "add";
		}

		try {
			etudiantService.create(etudiant);
			redirectAttributes.addFlashAttribute("message", "L'étudiant a bien été enregistré");

		} catch (ResourceAlreadyExistsException e) {
			logger.error("{}: {}", ErrorType.CREATION_ERROR.name(), e.getMessage());
			model.addAttribute("error", e.getMessage());
			model.addAttribute("encadreurs", encadreurService.list());
			model.addAttribute("stages", stageService.list());
			model.addAttribute("activePage", "etudiant");

			return prefix + "add";

		} catch (Exception e) {
			logger.error("{}: {}", ErrorType.CREATION_ERROR.name(), e.getMessage());
			redirectAttributes.addFlashAttribute("error", ErrorType.CREATION_ERROR.get());
		}

		return "redirect:/etudiant";
	}

	@GetMapping("edit/{id}")
	public String updateForm(@PathVariable("id") Long id, Model model, RedirectAttributes redirectAttributes) {

		try {
			model.addAttribute("etudiant", etudiantService.find(id));
			model.addAttribute("encadreurs", encadreurService.list());
			model.addAttribute("stages", stageService.list());
			model.addAttribute("activePage", "etudiant");

		} catch (Exception e) {
			logger.error("{}: {}", ErrorType.CHARGEMENT_ERROR.name(), e.getMessage());
			redirectAttributes.addFlashAttribute("error", ErrorType.CHARGEMENT_ERROR.get());

			return "redirect:/etudiant";
		}

		return prefix + "update";
	}

	@PostMapping("update")
	public String update(@Valid @ModelAttribute("etudiant") Etudiant etudiant, BindingResult bindingResult, Model model,
			RedirectAttributes redirectAttributes) {

		if (bindingResult.hasErrors()) {
			model.addAttribute("encadreurs", encadreurService.list());
			model.addAttribute("stages", stageService.list());
			model.addAttribute("activePage", "etudiant");

			return prefix + "update";
		}

		try {
			etudiantService.update(etudiant, etudiant.getId());
			redirectAttributes.addFlashAttribute("message", "L'étudiant a bien été modifié");

		} catch (ResourceNotFoundException | ResourceAlreadyExistsException e) {
			logger.error("{}: {}", ErrorType.MODIFICATION_ERROR.name(), e.getMessage());
			model.addAttribute("error", e.getMessage());
			model.addAttribute("encadreurs", encadreurService.list());
			model.addAttribute("stages", stageService.list());
			model.addAttribute("activePage", "etudiant");

			return prefix + "update";

		} catch (Exception e) {
			logger.error("{}: {}", ErrorType.MODIFICATION_ERROR.name(), e.getMessage());
			redirectAttributes.addFlashAttribute("error", ErrorType.MODIFICATION_ERROR.get());
		}

		return "redirect:/etudiant";
	}

	@GetMapping("details/{id}")
	public String details(@PathVariable("id") Long id, Model model, RedirectAttributes redirectAttributes) {

		try {
			model.addAttribute("etudiant", etudiantService.find(id));
			model.addAttribute("activePage", "etudiant");

		} catch (Exception e) {
			logger.error("{}: {}", ErrorType.CHARGEMENT_ERROR.name(), e.getMessage());
			redirectAttributes.addFlashAttribute("error", ErrorType.CHARGEMENT_ERROR.get());

			return "redirect:/etudiant";
		}

		return prefix + "details";
	}

	@GetMapping("delete/{id}")
	public String delete(@PathVariable("id") Long id, RedirectAttributes redirectAttributes) {

		try {
			etudiantService.delete(id);
			redirectAttributes.addFlashAttribute("message", "L'étudiant " + id + " a bien été supprimé");

		} catch (ResourceNotFoundException e) {
			logger.error("{}: {}", ErrorType.SUPPRESSION_ERROR.name(), e.getMessage());
			redirectAttributes.addFlashAttribute("error", e.getMessage());

		} catch (Exception e) {
			logger.error("{}: {}", ErrorType.SUPPRESSION_ERROR.name(), e.getMessage());
			redirectAttributes.addFlashAttribute("error", ErrorType.SUPPRESSION_ERROR.get());
		}

		return "redirect:/etudiant";
	}

}
