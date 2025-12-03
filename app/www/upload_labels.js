(function() {
  // Register once Shiny is available
  function register() {
    if (typeof Shiny === "undefined" || !Shiny.addCustomMessageHandler) {
      // Retry shortly if Shiny isn't ready yet
      setTimeout(register, 50);
      return;
    }

    Shiny.addCustomMessageHandler("updateFileInputLabels", function(message) {
      // message = { id: <hidden file input id>, buttonLabel, placeholder }
      // IMPORTANT: 'id' must be the ID of the *hidden* <input type="file">,
      // i.e., the namespaced inputId from R: session$ns("csv")
      try {
        var hiddenInput = document.getElementById(message.id);
        if (!hiddenInput) return;

        // The visible controls live in the nearest .input-group
        var container = hiddenInput.closest(".input-group");
        if (!container) return;

        // Update placeholder on the readonly text input (visible "filename" box)
        var textInput = container.querySelector("input.form-control[readonly]");
        if (textInput && typeof message.placeholder === "string") {
          textInput.setAttribute("placeholder", message.placeholder);
        }

        // Update the button label text
        // In your markup, the button is the .btn-file <span> itself
        var btnSpan = container.querySelector(".btn-file");
        if (btnSpan && typeof message.buttonLabel === "string") {
          var fileInput = btnSpan.querySelector('input[type="file"]');
          if (fileInput) {
            // Ensure a dedicated label node exists
            var labelNode = btnSpan.querySelector('.btn-file-label');
            if (!labelNode) {
              labelNode = document.createElement('span');
              labelNode.className = 'btn-file-label';
              btnSpan.insertBefore(labelNode, fileInput); // insert before the input
            }
            labelNode.textContent = message.buttonLabel;
          }
        }
      } catch (e) {
        if (console && console.warn) {
          console.warn("updateFileInputLabels failed:", e);
        }
      }
    });
  }

  // Attempt registration now; will retry until Shiny is ready
  register();
})();

