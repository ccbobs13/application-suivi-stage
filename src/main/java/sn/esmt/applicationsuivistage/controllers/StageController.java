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
import sn.esmt.applicationsuivistage.entities.Stage;
import sn.esmt.applicationsuivistage.enums.ErrorType;
import sn.esmt.applicationsuivistage.exceptions.ResourceAlreadyExistsException;
import sn.esmt.applicationsuivistage.exceptions.ResourceNotFoundException;
import sn.esmt.applicationsuivistage.services.StageService;

@Controller
@RequestMapping("/stage")
public class StageController {

	@Autowired
	private StageService service;

	private String prefix = "views/stage/";

	private static final Logger logger = LoggerFactory.getLogger(StageController.class);

	@GetMapping
	public String index(@RequestParam(name = "page", defaultValue = "1") int page,
			@RequestParam(name = "size", defaultValue = "5") int size,
			@RequestParam(name = "keyword", required = false) String keyword, Model model) {
		try {

			Page<Stage> pageStage;

			if (keyword == null) {
				pageStage = service.list(page - 1, size);
				model.addAttribute("stages", pageStage.getContent());
			} else {
				pageStage = service.listByThemeContaining(page - 1, size, keyword);
				model.addAttribute("stages", pageStage.getContent());
				model.addAttribute("keyword", keyword);
			}

			model.addAttribute("pages", pageStage.getTotalPages());
			model.addAttribute("pageSize", size);
			model.addAttribute("currentPage", pageStage.getNumber() + 1);
			model.addAttribute("activePage", "stage");

		} catch (Exception e) {
			logger.error("{}: {}", ErrorType.CHARGEMENT_ERROR.name(), e.getMessage());
			model.addAttribute("error", ErrorType.CHARGEMENT_ERROR.get());
		}
		return prefix + "index";

	}

	@GetMapping("add")
	public String addForm(Model model) {
		model.addAttribute("stage", new Stage());
		model.addAttribute("activePage", "stage");

		return prefix + "add";
	}

	@PostMapping("save")
	public String save(@Valid @ModelAttribute("stage") Stage stage, BindingResult bindingResult, Model model,
			RedirectAttributes redirectAttributes) {

		if (bindingResult.hasErrors()) {
			model.addAttribute("activePage", "stage");

			return prefix + "add";
		}

		try {
			service.create(stage);
			redirectAttributes.addFlashAttribute("message", "Le stage a bien été enregistré");

		} catch (ResourceAlreadyExistsException e) {
			logger.error("{}: {}", ErrorType.CREATION_ERROR.name(), e.getMessage());
			model.addAttribute("error", e.getMessage());
			model.addAttribute("activePage", "stage");

			return prefix + "add";

		} catch (Exception e) {
			logger.error("{}: {}", ErrorType.CREATION_ERROR.name(), e.getMessage());
			redirectAttributes.addFlashAttribute("error", ErrorType.CREATION_ERROR.get());
		}

		return "redirect:/stage";
	}

	@GetMapping("edit/{id}")
	public String updateForm(@PathVariable("id") Long id, Model model, RedirectAttributes redirectAttributes) {
		try {
			model.addAttribute("stage", service.find(id));
			model.addAttribute("activePage", "stage");

		} catch (Exception e) {
			logger.error("{}: {}", ErrorType.CHARGEMENT_ERROR.name(), e.getMessage());
			redirectAttributes.addFlashAttribute("error", ErrorType.CHARGEMENT_ERROR.get());

			return "redirect:/stage";

		}
		return prefix + "update";
	}

	@PostMapping("update")
	public String update(@Valid @ModelAttribute("stage") Stage stage, BindingResult bindingResult, Model model,
			RedirectAttributes redirectAttributes) {

		if (bindingResult.hasErrors()) {
			model.addAttribute("activePage", "stage");

			return prefix + "update";
		}

		try {
			service.update(stage, stage.getId());
			redirectAttributes.addFlashAttribute("message", "Le stage a bien été modifié");

		} catch (ResourceNotFoundException | ResourceAlreadyExistsException e) {
			logger.error("{}: {}", ErrorType.MODIFICATION_ERROR.name(), e.getMessage());
			model.addAttribute("error", e.getMessage());
			model.addAttribute("activePage", "stage");

			return prefix + "update";

		} catch (Exception e) {
			logger.error("{}: {}", ErrorType.MODIFICATION_ERROR.name(), e.getMessage());
			redirectAttributes.addFlashAttribute("error", ErrorType.MODIFICATION_ERROR.get());
		}

		return "redirect:/stage";
	}

	@GetMapping("delete/{id}")
	public String delete(@PathVariable("id") Long id, RedirectAttributes redirectAttributes) {
		try {
			service.delete(id);
			redirectAttributes.addFlashAttribute("message", "Le stage " + id + " a bien ete supprime");

		} catch (ResourceNotFoundException e) {
			logger.error("{}: {}", ErrorType.SUPPRESSION_ERROR.name(), e.getMessage());
			redirectAttributes.addFlashAttribute("error", e.getMessage());

		} catch (Exception e) {
			logger.error("{}: {}", ErrorType.SUPPRESSION_ERROR.name(), e.getMessage());
			redirectAttributes.addFlashAttribute("error", ErrorType.SUPPRESSION_ERROR.get());
		}

		return "redirect:/stage";
	}

	@GetMapping("details/{id}")
	public String details(@PathVariable("id") Long id, Model model, RedirectAttributes redirectAttributes) {
		try {
			model.addAttribute("stage", service.find(id));
			model.addAttribute("activePage", "stage");

		} catch (Exception e) {
			logger.error("{}: {}", ErrorType.CHARGEMENT_ERROR.name(), e.getMessage());
			redirectAttributes.addFlashAttribute("error", ErrorType.CHARGEMENT_ERROR.get());

			return "redirect:/stage";

		}
		return prefix + "details";
	}

}
