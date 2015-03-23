{-------------------------------------------------------------------------------+
|                                                                               |
|  TQRMultiExport - The QuickReport export component                            |
|                                                                               |
|  Description         Export your QuickReport outputs to image files or PDF    |
|                                                                               |
|  Author              Toci                                                     |
|  MailTo              toth.akos@interware.hu                                   |
|                                                                               |
|  Version             1.0                                                      |
|  Date                25/01/2008                                               |
|  Target              Delphi7, you may try other                               |
|                                                                               |
|  Supported formats   PDF, JPEG, GIF, BMP, WMF, EMF, (TIFF)                    |
|                                                                               |
|  License             Free under the terms of the GPL                          |
|                      (General Public License)                                 |
|                                                                               |
+-------------------------------------------------------------------------------}

Description
This component allows you to save your reports in different graphics formats.
Supported formats are:

  - Adobe Acrobat Document  - the library by K. Nishita
  - GIF Image               - the component by Anders Melander
  - JPEG image              - Delphi built-in
  - Bitmap                  - Delphi built-in
  - EMF Image               - Delphi built-in 
  - Windows Meta File       - Delphi built-in
  - TIFF image              - the component by Nick Chislin, having some bugs

Install
 a) Third party components should be installed separately, see information there
 b) In Delphi select Component / Install Component, browse for TQRMultiExport.pas
    and click OK

Use
Drop the TQRMultiExport component onto a form and configure it. The component can
automatically configure and display it's "Save as..." dialog with a file filter list
configured according to the user's registry settings.


Start export

  QRMultiExport1.DoExport;


Properties (in alphabetical order)

  Compression   - Integer; JPEG format's compression quality - also applyed for PDF
                  See the help for TJPEGImage

  DialogTitle   - String; Title of the TSaveDialog window

  DPI           - Real; Image resolution. When 0, MinHeight and MinWidth are used
                  Other way MinHeight and MinWidth are calculated from Report 
                  size in inches and DPI

  ExportFormat  - TQRExportFormat; See supporded format list

  FileName      - String; Export file name with path. Default is 'Export'. Extension will be 
                  assigned automatically. Multipage image files are saved as Export0000n.*

  MinHeight     - Integer; When DPI is 0, the output's width will be at least MinWidth

  MinWidth      - Integer; When DPI is 0, the output's height will be at least MinHeight

  Name          - ShortString; Component's name

  PixelFormat   - PixelFormat for BMP and TIFF

  Report        - TQuickReport; The report to export

  ShowDialog    - Boolean; Show "Save as..." dialog, or not

Events

  OnBeginExport  - Event will be fired after the user closes the "Save as..." dialog  

  OnFinishExport - Event will be fired after all pages are exported

  OnPage         - Event will be fired before exporting the page. Event returns page number and file name

  OnPrepare      - Event will be fired after preparing the report. Event returns the number of pages

