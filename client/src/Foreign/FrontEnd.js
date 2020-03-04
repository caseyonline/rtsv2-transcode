
exports.init_ = function() {
     /* Aside: dropdown toggle */

  Array.from(document.getElementsByClassName('jb-dropdown')).forEach(
    function(el) {
      el.addEventListener('click', function(e) {
        const dropdownIcon = e.currentTarget
          .getElementsByClassName('submenu-icon')[0]
          .getElementsByClassName('mdi')[0]

        e.currentTarget.parentNode.classList.toggle('is-active')
        dropdownIcon.classList.toggle('mdi-plus')
        dropdownIcon.classList.toggle('mdi-minus')
      })
    })

    /* Aside Desktop toggle */
    Array.from(document.getElementsByClassName('jb-aside-toggle')).forEach(
    function(el) {
      el.addEventListener('click', function(e) {
        const dropdownIcon = e.currentTarget
          .getElementsByClassName('icon')[0]
          .getElementsByClassName('mdi')[0]

        document.getElementById(e.currentTarget.getAttribute('data-target')).classList.toggle('is-expanded')

        document.documentElement.classList.toggle('has-aside-expanded')
        dropdownIcon.classList.toggle('mdi-forwardburger')
        dropdownIcon.classList.toggle('mdi-backburger')
      })
    })

    /* Aside Mobile toggle */
    Array.from(document.getElementsByClassName('jb-aside-mobile-toggle')).forEach(
    function(el) {
      el.addEventListener('click', function(e) {
        const dropdownIcon = e.currentTarget
            .getElementsByClassName('icon')[0]
            .getElementsByClassName('mdi')[0]

        document.documentElement.classList.toggle('has-aside-mobile-expanded')
        dropdownIcon.classList.toggle('mdi-forwardburger')
        dropdownIcon.classList.toggle('mdi-backburger')
      })
    })

    /* Aside right toggle */
    Array.from(document.getElementsByClassName('jb-aside-right-toggle')).forEach(
    function(el) {
      el.addEventListener('click', function(e) {
        e.preventDefault()
        document.documentElement.classList.toggle('has-aside-right')
      })
    })

    /* NavBar menu mobile toggle */
    Array.from(document.getElementsByClassName('jb-navbar-menu-toggle')).forEach(
    function(el) {
      el.addEventListener('click', function(e) {
        const dropdownIcon = e.currentTarget
            .getElementsByClassName('icon')[0]
            .getElementsByClassName('mdi')[0]

        document.getElementById(e.currentTarget.getAttribute('data-target')).classList.toggle('is-active')
        dropdownIcon.classList.toggle('mdi-dots-vertical')
        dropdownIcon.classList.toggle('mdi-close')
      })
    })

    /* Modal: open */
    Array.from(document.getElementsByClassName('jb-modal')).forEach(
    function(el) {
      el.addEventListener('click', function(e) {
        const modalTarget = e.currentTarget.getAttribute('data-target')

        document.getElementById(modalTarget).classList.add('is-active')
        document.documentElement.classList.add('is-clipped')
      })
    });

    /* Modal: close */
    Array.from(document.getElementsByClassName('jb-modal-close')).forEach(
    function(el) {
      el.addEventListener('click', function(e) {
        e.currentTarget.closest('.modal').classList.remove('is-active')
        document.documentElement.classList.remove('is-clipped')
      })
    })

    /* Notification dismiss */
    Array.from(document.getElementsByClassName('jb-notification-dismiss')).forEach(
    function(el) {
      el.addEventListener('click', function(e) {
        e.currentTarget.closest('.notification').classList.add('is-hidden')
      })
    })

    /* Aside: submenus */
  const asideSecondaryToggle = function(e) {
      //e.preventDefault()

      const asideMain = document.getElementById('aside-main')

      Array.from(asideMain.getElementsByClassName('is-active-original')).concat(Array.from(asideMain.getElementsByClassName('is-active'))).forEach(elA => {
        if (!elA.classList.contains('jb-aside-secondary-toggle')) {
          if (elA.classList.contains('is-active-original')) {
            elA.classList.remove('is-active-original')
            elA.classList.add('is-active')
          } else {
            elA.classList.remove('is-active')
            elA.classList.add('is-active-original')
          }
        }
      })

      if (e.currentTarget.classList.contains('jb-aside-secondary-toggle')) {
        e.currentTarget.classList.toggle('is-active')
      } else {
        Array.from(asideMain.getElementsByClassName('jb-aside-secondary-toggle')).forEach(elA => {
          elA.classList.remove('is-active')
        })
      }

      document.getElementById('aside-secondary').classList.toggle('is-hidden')
      //document.getElementById('ui-overlay').classList.toggle('is-hidden')
      asideMain.classList.toggle('has-secondary')
    }

    Array.from(document.getElementsByClassName('jb-aside-secondary-toggle')).forEach(
    function(el) {
      el.addEventListener('click', asideSecondaryToggle)
    })

    Array.from(document.getElementsByClassName('aside-close')).forEach(
    function(el) {
      el.addEventListener('click', asideSecondaryToggle)
    })

    Array.from(document.getElementsByClassName('aside-secondary-li')).forEach(
    function(el) {
      el.addEventListener('click', asideSecondaryToggle)
    })

    /* Ui overlay */
    // document.getElementById('ui-overlay').addEventListener('click', asideSecondaryToggle)
};
