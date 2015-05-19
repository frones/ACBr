{$I ACBr.inc}

unit ACBrNFSeDANFSeRVClass;

interface

uses
 Forms, SysUtils, Classes, pnfsNFSe, ACBrNFSeDANFSeClass, RpDefine, 
 pnfsConversao, DANFSeRaveDM, StrUtils, Dialogs, ACBrUtil, ACBrNFSe;

type
  TACBrNFSeDANFSeRV = class( TACBrNFSeDANFSeClass )
   private
     FdmDanfse : TDANFSeDM;
     FRaveFile : String;
   public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ImprimirDANFSe(NFSe : TNFSe = nil); override ;
    procedure ImprimirDANFSePDF(NFSe : TNFSe = nil); override ;
  published
    property RavFile : String read FRaveFile write FRaveFile;
    property dmDanfse: TDANFSeDM  read FdmDanfse write FdmDanfse ;
  end;

var
  ACBrNFSeDANFSeRV : TACBrNFSeDANFSeRV ;

implementation

constructor TACBrNFSeDANFSeRV.Create(AOwner: TComponent);
begin
  inherited create( AOwner );
  dmDanfse  := TDANFSeDM.Create(Self) ;
  FRaveFile := '' ;
end;

destructor TACBrNFSeDANFSeRV.Destroy;
begin
  dmDanfse.Free ;
  inherited Destroy ;
end;


procedure TACBrNFSeDANFSeRV.ImprimirDANFSe(NFSe : TNFSe = Nil);
var
  i : integer ;
begin
  {$IFDEF RAVE50VCL}
     RPDefine.DataID := IntToStr(Application.Handle);  // Evita msg de erro;...
  {$ENDIF}

  i := TACBrNFSe(ACBrNFSe).NotasFiscais.Count - 1 ;

  if FRaveFile = '' then
    raise Exception.Create(' Arquivo de Relatório não informado.')
  else
  begin
    if not FilesExists(FRaveFile) then
      raise Exception.Create('Arquivo '+FRaveFile+' Não encontrado, no caminho indicado');

    dmDanfse.RvProject.ClearRaveBlob;
    dmDanfse.RvProject.ProjectFile := FRaveFile;
  end;

  dmDanfse.RvSystem1.DoNativeOutput := True;
  dmDanfse.RvRenderPDF1.Active := True;
  if MostrarPreview then
  begin
    dmDanfse.RvSystem1.DefaultDest    := rdPreview;
    dmDanfse.RvSystem1.SystemSetups:=dmDanfse.RvSystem1.SystemSetups + [ssAllowSetup];
  end
  else
  begin
    dmDanfse.RvSystem1.DefaultDest    := rdPrinter;
    dmDanfse.RvSystem1.SystemSetups:=dmDanfse.RvSystem1.SystemSetups - [ssAllowSetup];
  end;
  dmDanfse.RvSystem1.RenderObject   := nil;
  dmDanfse.RvSystem1.OutputFileName := '';
  dmDanfse.RvProject.Engine := dmDanfse.RvSystem1;
  dmDanfse.RvSystem1.TitlePreview:='Visualizar DANFSE';
  dmDanfse.RvSystem1.TitleSetup:='Configurações';
  dmDanfse.RvSystem1.TitleStatus:='Status da Impressão';
  dmDanfse.RvSystem1.SystemFiler.StatusFormat:='Gerando página %p';
  dmDanfse.RvSystem1.SystemFiler.StreamMode:=smMemory;
  dmDanfse.RvSystem1.SystemOptions:=[soShowStatus,soAllowPrintFromPreview,soPreviewModal];
  if not MostrarStatus then
     dmDanfse.RvSystem1.SystemOptions:=dmDanfse.RvSystem1.SystemOptions - [soShowStatus];
  dmDanfse.RvSystem1.SystemPreview.FormState := wsMaximized;
  dmDanfse.RvSystem1.SystemPreview.ZoomFactor:=100;
  dmDanfse.RvSystem1.SystemPrinter.Copies:=NumCopias;
  dmDanfse.RvSystem1.SystemPrinter.LinesPerInch:=8;
  dmDanfse.RvSystem1.SystemPrinter.StatusFormat:='Imprimindo página %p';
  dmDanfse.RvSystem1.SystemPrinter.Title:= 'NFSe - Impressão do DANFSE';
  dmDanfse.RvSystem1.SystemPrinter.Units:=unMM;
  dmDanfse.RvSystem1.SystemPrinter.UnitsFactor:=25.4;

  if NFSe = nil then
    NFSe := TACBrNFSe(ACBrNFSe).NotasFiscais.Items[i].NFSe ;

  dmDanfse.NFSe  := NFSe ;

  dmDanfse.nPref01 := '' ;
  dmDanfse.nImagem := '' ;
  dmDanfse.nDatHor := '' ;
  dmDanfse.Sistema := '' ;
  dmDanfse.nLogoPr := '' ;
  dmDanfse.nOutras := '' ;

  for i := 1 to length(Self.Prefeitura) do
     if Copy(Self.Prefeitura,i,1) = ';' then
       dmDanfse.nPref01 := dmDanfse.nPref01 + #13
     else
       dmDanfse.nPref01 := dmDanfse.nPref01 + Copy(Self.Prefeitura,i,1) ;

  dmDanfse.nImagem := Self.Logo ;
  dmDanfse.nLogoPr := Self.PrestLogo ;
  dmDanfse.nDatHor := 'Data e Hora da Impressão: '+DateTimeToStr(Now) ;
  dmDanfse.Sistema := 'Desenvolvido por: '+Self.Sistema ;

  if TACBrNFSe(ACBrNFSe).Configuracoes.WebServices.AmbienteCodigo = 2 then
    dmDanfse.nOutras := 'NOTA FISCAL SEM VALOR FISCAL - AMBIENTE DE HOMOLOGAÇÃO'
  else
    dmDanfse.nOutras := '' ;

  dmDanfse.RvProject.Execute ;

end;

procedure TACBrNFSeDANFSeRV.ImprimirDANFSePDF(NFSe : TNFSe = nil);
var
  NomeArq : String;
  i : integer ;
begin
  {$IFDEF RAVE50VCL}
     RPDefine.DataID := IntToStr(Application.Handle);  // Evita msg de erro;...
  {$ENDIF}

  i := TACBrNFSe(ACBrNFSe).NotasFiscais.Count - 1 ;

   if FRaveFile = '' then
     raise Exception.Create(' Arquivo de Relatório nao informado.')
   else
   begin
      if not FilesExists(FRaveFile) then
        raise Exception.Create('Arquivo '+FRaveFile+' Nao encontrado');

      dmDanfse.RvProject.ClearRaveBlob;
      dmDanfse.RvProject.ProjectFile := FRaveFile;  //ExtractFileDir(application.ExeName)+'\Report\NotaFiscalEletronica.rav';
   end;

  dmDanfse.RvSystem1.DefaultDest := rdFile;
  dmDanfse.RvSystem1.DoNativeOutput:=false;
  dmDanfse.RvSystem1.RenderObject:= dmDanfse.RvRenderPDF1;
  if not MostrarStatus then
     dmDanfse.RvSystem1.SystemOptions:=dmDanfse.RvSystem1.SystemOptions - [soShowStatus];
  dmDanfse.RvSystem1.SystemSetups:=dmDanfse.RvSystem1.SystemSetups - [ssAllowSetup];
  dmDanfse.RvProject.Engine := dmDanfse.RvSystem1;
  dmDanfse.RvRenderPDF1.EmbedFonts:=False;
  dmDanfse.RvRenderPDF1.ImageQuality:=90;
  dmDanfse.RvRenderPDF1.MetafileDPI:=300;
  dmDanfse.RvRenderPDF1.UseCompression:=False;
  dmDanfse.RvRenderPDF1.Active:=True;

  if NFSe = nil then
    NFSe := TACBrNFSe(ACBrNFSe).NotasFiscais.Items[i].NFSe ;

  dmDanfse.NFSe := NFSE ;

  if TACBrNFSe(ACBrNFSe).Configuracoes.Arquivos.NomeLongoNFSe then
    NomeArq := NotaUtil.GerarNomeNFSe(StrToInt(Copy(NFSe.PrestadorServico.Endereco.CodigoMunicipio,1,2)),
                                      NFSe.DataEmissao,
                                      NFSe.PrestadorServico.IdentificacaoPrestador.Cnpj,
                                      StrToIntDef(NFSe.Numero, 0))
  else
    NomeArq := NFSe.Numero;

  NomeArq := StringReplace(NomeArq,'NFSe', '', [rfIgnoreCase]);
  NomeArq := PathWithDelim(Self.PathPDF)+StringReplace(NomeArq,'A','', [rfIgnoreCase])+'.pdf';

  dmDanfse.nPref01 := '' ;
  dmDanfse.nImagem := '' ;
  dmDanfse.nDatHor := '' ;
  dmDanfse.Sistema := '' ;
  dmDanfse.nLogoPr := '' ;
  dmDanfse.nOutras := '' ;

  dmDanfse.RvSystem1.OutputFileName := NomeArq;

  for i := 1 to length(Self.Prefeitura) do
     if Copy(Self.Prefeitura,i,1) = ';' then
       dmDanfse.nPref01 := dmDanfse.nPref01 + #13
     else
       dmDanfse.nPref01 := dmDanfse.nPref01 + Copy(Self.Prefeitura,i,1) ;

  dmDanfse.nImagem := Self.Logo ;
  dmDanfse.nLogoPr := Self.PrestLogo ;
  dmDanfse.nDatHor := 'Data e Hora da Impressão: '+DateTimeToStr(Now) ;
  dmDanfse.Sistema := 'Desenvolvido por: '+Self.Sistema ;

  if TACBrNFSe(ACBrNFSe).Configuracoes.WebServices.AmbienteCodigo = 2 then
    dmDanfse.nOutras := 'NOTA FISCAL SEM VALOR FISCAL - AMBIENTE DE HOMOLOGAÇÃO'
  else
    dmDanfse.nOutras := '' ;

  dmDanfse.RvProject.Execute ;

  dmDanfse.RvRenderPDF1.Active:=False;

end;

end.
