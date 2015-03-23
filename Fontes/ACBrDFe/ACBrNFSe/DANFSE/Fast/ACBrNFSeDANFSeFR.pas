unit ACBrNFSeDANFSeFR;

interface

uses
  Forms, SysUtils, Classes, Graphics, ACBrNFSeDANFSeClass, ACBrNFSeDANFSeFRDM,
  pnfsNFSe, {pcnConversao,} pnfsConversao, frxClass;

type
  EACBrNFSeDANFSeFR = class(Exception);

  TACBrNFSeDANFSeFR = class( TACBrNFSeDANFSeClass )
   private
     FdmDANFSe: TdmACBrNFSeFR;
     FFastFile: String;
     FEspessuraBorda: Integer;
     function GetPreparedReport: TfrxReport;
     function PrepareReport(NFSe: TNFSe = nil): Boolean;
   public
     constructor Create(AOwner: TComponent); override;
     destructor Destroy; override;
     procedure ImprimirDANFSe(NFSe: TNFSe = nil); override;
     procedure ImprimirDANFSePDF(NFSe: TNFSe = nil); override;
   published
     property FastFile: String read FFastFile write FFastFile;
     property dmDanfse: TdmACBrNFSeFR read FdmDANFSe write FdmDANFSe;
     property EspessuraBorda: Integer read FEspessuraBorda write FEspessuraBorda;
     property PreparedReport: TfrxReport read GetPreparedReport;
  end;

implementation

uses
  ACBrNFSe, ACBrUtil, StrUtils, Dialogs;

constructor TACBrNFSeDANFSeFR.Create(AOwner: TComponent);
begin
  inherited Create( AOwner );
  dmDanfse := TdmACBrNFSeFR.Create(Self);
  FFastFile := '';
  FEspessuraBorda := 1;
end;

destructor TACBrNFSeDANFSeFR.Destroy;
begin
  dmDanfse.Free;
  inherited Destroy;
end;

function TACBrNFSeDANFSeFR.GetPreparedReport: TfrxReport;
begin
  if Trim(FFastFile) = '' then
    Result := nil
  else
  begin
    if PrepareReport(nil) then
      Result := dmDanfse.frxReport
    else
      Result := nil;
  end;
end;

procedure TACBrNFSeDANFSeFR.ImprimirDANFSe(NFSe: TNFSe);
begin
  if PrepareReport(NFSe) then
  begin
    if MostrarPreview then
      dmDanfse.frxReport.ShowPreparedReport
    else
      dmDanfse.frxReport.Print;
  end;
end;

procedure TACBrNFSeDANFSeFR.ImprimirDANFSePDF(NFSe: TNFSe);
const
  TITULO_PDF = 'Nota Fiscal de Serviço Eletrônica';
var
  I: Integer;
begin
  if PrepareReport(NFSe) then
  begin
    dmDanfse.frxPDFExport.Author        := Sistema;
    dmDanfse.frxPDFExport.Creator       := Sistema;
//    dmDanfse.frxPDFExport.Producer      := Sistema;
//    dmDanfse.frxPDFExport.Title         := TITULO_PDF;
    dmDanfse.frxPDFExport.Subject       := TITULO_PDF;
//    dmDanfse.frxPDFExport.Keywords      := TITULO_PDF;
    dmDanfse.frxPDFExport.ShowDialog    := False;

    for I := 0 to TACBrNFSe(ACBrNFSe).NotasFiscais.Count -1 do
    begin
//      dmDanfse.frxPDFExport.FileName := PathPDF+ dmDanfse.NFSe.Numero+dmDanfse.NFSe.CodigoVerificacao+'.pdf';
      dmDanfse.frxPDFExport.FileName := PathPDF+ dmDanfse.NFSe.Numero+'.pdf';
      dmDanfse.frxReport.Export(dmDanfse.frxPDFExport);
    end;
  end;
end;

function TACBrNFSeDANFSeFR.PrepareReport(NFSe: TNFSe): Boolean;
var
  i: Integer;
begin
  Result := False;
  
  if Trim(FastFile) <> '' then
  begin
    if FileExists(FastFile) then
      dmDanfse.frxReport.LoadFromFile(FastFile)
    else
      raise EACBrNFSeDANFSeFR.CreateFmt('Caminho do arquivo de impressão do DANFSe "%s" inválido.', [FastFile]);
  end
  else
    raise EACBrNFSeDANFSeFR.Create('Caminho do arquivo de impressão do DANFSe não assinalado.');

  dmDanfse.frxReport.PrintOptions.Copies := NumCopias;
  dmDanfse.frxReport.PrintOptions.ShowDialog := MostrarPreview;
  dmDanfse.frxReport.ShowProgress := Self.MostrarStatus;

  // Incluído por Luciano Enzweiler em 23/01/2013
  // Define a impressora
  if Impressora > '' then
  begin
    dmDanfse.frxReport.PrintOptions.ShowDialog := False;
    dmDanfse.frxReport.PrintOptions.Printer := Impressora;
  end;

  if Assigned(NFSe) then
  begin
    dmDanfse.NFSe := NFSe;
    dmDanfse.CarregaDados;

    Result := dmDanfse.frxReport.PrepareReport;
  end
  else
  begin
    if Assigned(ACBrNFSe) then
    begin
      for i := 0 to TACBrNFSe(ACBrNFSe).NotasFiscais.Count - 1 do
      begin
        dmDanfse.NFSe := TACBrNFSe(ACBrNFSe).NotasFiscais.Items[i].NFSe;
        dmDanfse.CarregaDados;

        if (i > 0) then
          Result := dmDanfse.frxReport.PrepareReport(False)
        else
          Result := dmDanfse.frxReport.PrepareReport;
          
      end;
    end
    else
      raise EACBrNFSeDANFSeFR.Create('Propriedade ACBrNFSe não assinalada.');
  end;      
end;

end.
