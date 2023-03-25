document.addEventListener("DOMContentLoaded", () => {
	let year = document.getElementById("year");
	year.textContent = new Date().getFullYear();

	let pageSize = document.getElementById('pageSize');
	let searchForm = document.getElementById("searchForm");
	let searchInput = document.getElementById("searchInput");

	if (searchForm != null) {
		searchForm.addEventListener("submit", (e) => {
			e.preventDefault();
			if (searchForm != null && searchInput.value?.trim() != '') {
				searchForm.submit();
			}
		});
	}


	if (pageSize != null) {
		pageSize.addEventListener('change', () => {
			if (searchForm != null) {
				searchForm.submit();
			}
		});
	}

	if (searchInput != null) {
		searchInput.addEventListener("search", () => {
			if (searchForm != null) {
				searchForm.submit();
			}
		});
	}
});
