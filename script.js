document.addEventListener("DOMContentLoaded", function () {
  const sections = document.querySelectorAll(".content-section");
  const navLinks = document.querySelectorAll(".sidebar ul li a");
  const hamburger = document.querySelector(".hamburger");
  const sidebar = document.querySelector(".sidebar");

  // Show initial section
  function showSection(sectionId) {
    sections.forEach(section => section.style.display = "none");
    document.getElementById(sectionId).style.display = "block";

    // Remove active class from all links
    navLinks.forEach(link => link.classList.remove("active"));

    // Highlight active section link
    const activeLink = document.querySelector(`.sidebar ul li a[href="#"][onclick="showSection('${sectionId}')"]`);
    if (activeLink) activeLink.classList.add("active");
  }

  // Attach event listeners to menu items
  navLinks.forEach(link => {
    link.addEventListener("click", function (event) {
      event.preventDefault();
      const sectionId = this.getAttribute("onclick").match(/'([^']+)'/)[1];
      showSection(sectionId);
    });
  });

  // Toggle sidebar on hamburger menu click
  hamburger.addEventListener("click", function () {
    sidebar.classList.toggle("active");
  });

  // Only one sidebar menu open at a time
  const toctreeL1 = document.querySelectorAll('.toctree-l1 > a');
  toctreeL1.forEach(link => {
    link.addEventListener('click', function (e) {
      e.preventDefault();
      document.querySelectorAll('.toctree-l1').forEach(li => {
        if (li !== this.parentElement) {
          li.classList.remove('active');
        }
      });
      this.parentElement.classList.toggle('active');
    });
  });

  // Carousel functionality
 const carousels = document.querySelectorAll(".carousel");

  carousels.forEach(carousel => {
    let slideIndex = 0;
    const slides = carousel.querySelectorAll(".slide");
    const prev = carousel.querySelector(".prev");
    const next = carousel.querySelector(".next");

    function showSlides(n) {
      slides.forEach((slide, index) => {
        slide.style.display = (index === n) ? "block" : "none";
      });
    }

    function changeSlide(n) {
      slideIndex += n;
      if (slideIndex >= slides.length) {
        slideIndex = 0;
      } else if (slideIndex < 0) {
        slideIndex = slides.length - 1;
      }
      showSlides(slideIndex);
    }

    // Initialize: show first slide only
    showSlides(slideIndex);

    // Add event listeners
    if (prev) prev.addEventListener("click", () => changeSlide(-1));
    if (next) next.addEventListener("click", () => changeSlide(1));
  });
});