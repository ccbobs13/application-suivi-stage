document.addEventListener("DOMContentLoaded", () => {
	let deleteButtons = document.querySelectorAll("#deleteButton");
	let myModal = document.getElementById("staticBackdrop");
	let modalLabel = document.getElementById("staticBackdropLabel");
	let modalBody = document.getElementById("staticBackdropBody");
	let bsModal = new bootstrap.Modal(myModal);

	deleteButtons.forEach((btn) => {
		btn.addEventListener("click", (e) => {
			e.preventDefault();

			let yesButton = document.getElementById('yesButton');
			yesButton.setAttribute('href', btn.href);
			modalLabel.textContent = "Confirmation de dissociation";
			modalBody.innerText = "Êtes vous sûr de vouloir dissocier ?";
			bsModal.toggle();
		});
	});

});