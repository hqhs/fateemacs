;; -*- lexical-binding: t -*-

;; MLIR assembly. Upstream's own mlir-mode.el, vendored out of the LLVM tree
;; (mlir/utils/emacs/). It derives from `prog-mode', so the prog-conf.el setup
;; -- line numbers, show-paren, fill column, indent detection -- applies
;; without any extra wiring.

(use-package mlir-mode
  :mode (("\\.mlir\\'" . mlir-mode)
         ;; .mlirbc is MLIR bytecode. mlir-mode registers a jka-compr handler
         ;; that shells out to `mlir-opt' to round-trip it as text, so these
         ;; only read sensibly with mlir-opt on PATH -- and, since no autoloads
         ;; are generated for vendored packages, only once mlir-mode is already
         ;; loaded. The first .mlirbc opened in a session shows raw bytes;
         ;; revert the buffer and it decodes.
         ("\\.mlirbc\\'" . mlir-mode)))

(provide 'mlir)
