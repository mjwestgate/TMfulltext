# TMfulltext
Tools for importing full text of scientific articles from PDFs (via pdftools or OCR) and HTML.

## Functions
- read_fulltext_html for importing fulltext from HTML (online or stored as a file)
- process_ocr for converting PDF to text via OCR (note pdftools::pdf_text more efficient for recent PDFs; run_tesseract from this package for the underlying workhorse function)
- clean_double_columns for cleaning poorly formatted data imported by pdftools::pdf_text
- get_file_names for finding files in nested folders
