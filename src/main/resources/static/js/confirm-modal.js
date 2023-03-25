document.addEventListener("DOMContentLoaded", () => {
	let deleteButtons = document.querySelectorAll("#deleteButton");
	let myModal = document.getElementById("staticBackdrop");
	let bsModal = new bootstrap.Modal(myModal);

	deleteButtons.forEach((btn) => {
		btn.addEventListener("click", (e) => {
			e.preventDefault();

			let yesButton = document.getElementById('yesButton');
			yesButton.setAttribute('href', btn.href);

			bsModal.toggle();
		});
	});

});