{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2023 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }
{                                                                              }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }
{                                                                              }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

{$I ACBr.inc}

unit ACBr.DANFSe.FPDF.A4Retrato;

interface

uses
  Forms, 
  SysUtils, 
  Classes, 
  ACBrBase, 
  ACBrNFSeXClass,
  ACBr.DANFSeX.Classes,
  ACBr_fpdf,
  ACBr_fpdf_ext,
  ACBrUtil.FPDF;

type
  TACBrDANFSeFPDFA4Retrato = class(TObject)
  private
    FFPDF: TACBrFPDFExt;
    FLarguraTabela: Double;

    FNFSe: TNFSe;
    FDadosAuxDANFSe: TDadosNecessariosParaDANFSeX;
    FNomeArquivoPDFDestino: string;

    procedure ConfiguraDocumentoPDF;
    procedure ConfiguraPagina;

    procedure MontaCabecalhoNFSe(const AEspacoAntes: Double; AEspacoDepois: Double);
    procedure MontaDadosPrestadorServico(const AEspacoAntes: Double; AEspacoDepois: Double);
    procedure MontaDadosTomadorServico(const AEspacoAntes: Double; AEspacoDepois: Double);
    procedure MontaMensagensMarcaDAgua(const AEspacoAntes: Double; AEspacoDepois: Double);
    procedure MontaDadosDiscriminacaoServico(const AEspacoAntes: Double; AEspacoDepois: Double);
    procedure ImprimeCabecalhoDetalhesItensServico(const LargColunaDescricao,
        LargColunaValUnit, LargColunaQtde, LargColunaValServ, LargColunaBaseCalc,
        LargColunaISS: Double);
    procedure ImprimeUnicoItem;
    procedure ImprimeVariosItens;
    procedure MontaDetalhamentoEspecificoConstrucaoCivil(const AEspacoAntes: Double; AEspacoDepois: Double);
    procedure MontaDadosImpostosETotais(const AEspacoAntes: Double; AEspacoDepois: Double);
    procedure MontaOutrasInformacoes(const AEspacoAntes: Double; AEspacoDepois: Double);
    procedure MontaCanhotoReciboTomador(const AEspacoAntes: Double; AEspacoDepois: Double);
    procedure FinalizarArquivo;

  public
    constructor Create;
    destructor Destroy; override;

    procedure SalvarPDF(NFSe: TNFSe; DadosAuxDANFSe:
        TDadosNecessariosParaDANFSeX; UmACBrNFSeXDANFSe:TComponent;
        const NomeArquivoDestinoPDF: String);
//    TNotasFiscais
  end;

implementation

uses
  StrUtils,
  ACBrUtil.Base, ACBrUtil.FilesIO, ACBrValidador, ACBrNFSeXConversao,
  ACBrNFSeXDANFSeConsts, ACBrNFSeXDANFSeClass;

const
  TituloFonte = 'arial';
  TituloTamanhoFonte = 14;
  CabecalhoFonte = 'arial';
  CabecalhoTamanhoFonte = 9;
  IdentificadorTextoFonte = 'arial';
  IdentificadorTextoTamanhoFonte = 8;
  TextoFonte = 'arial';
  TextoTamanhoFonte = 8;
  TextoPequenoFonte = 'arial';
  TextoPequenoTamanhoFonte = 6;
  MargemInternaTabelas = 3;

var
  fpDANFSe : TACBrNFSeXDANFSeClass;

procedure DefineDanfse(OO: TComponent);
begin
  fpDANFSe := TACBrNFSeXDANFSeClass(OO);
end;

constructor TACBrDANFSeFPDFA4Retrato.Create;
begin
  inherited Create;
end;

destructor TACBrDANFSeFPDFA4Retrato.Destroy;
begin
  inherited Destroy;
end;

procedure TACBrDANFSeFPDFA4Retrato.SalvarPDF(NFSe: TNFSe; DadosAuxDANFSe:
    TDadosNecessariosParaDANFSeX; UmACBrNFSeXDANFSe:TComponent; const
    NomeArquivoDestinoPDF: String);
//Var
//  i: integer;
begin
  if NFSe = nil then
  begin
    raise EACBrException.Create('Parâmetro Inválido:NFSe não pode ser nil');
  end;
  if DadosAuxDANFSe = nil then
  begin
    raise EACBrException.Create('Parâmetro Inválido:DadosAuxDANFSe não pode ser nil');
  end;
  if not (UmACBrNFSeXDANFSe is TACBrNFSeXDANFSeClass) then
  begin
    raise EACBrException.Create('Parâmetro Inválido:"UmACBrNFSeXDANFSe" precisa ser do tipo "TACBrNFSeXDANFSeClass".');
  end;
  DefineDanfse(UmACBrNFSeXDANFSe);

  FNomeArquivoPDFDestino := NomeArquivoDestinoPDF;

  FNFSe := NFSe;
  FDadosAuxDANFSe := DadosAuxDANFSe;
  FFPDF := TACBrFPDFExt.Create(poPortrait, puMM, pfA4);
  try
    ConfiguraDocumentoPDF;
    ConfiguraPagina;
    FFPDF.AddPage();

    MontaCabecalhoNFSe(0, 0);
    MontaDadosPrestadorServico(0, 0);
    MontaDadosTomadorServico(0, 0);
    MontaMensagensMarcaDAgua(0, 0);
    MontaDadosDiscriminacaoServico(0, 0);
    if (FNFSe.ConstrucaoCivil.CodigoObra <> '') or (FNFSe.ConstrucaoCivil.Art <> '') then
    begin
      MontaDetalhamentoEspecificoConstrucaoCivil(0,0);
    end;
    MontaDadosImpostosETotais(0, 0);
    MontaOutrasInformacoes(0, 0);
    if fpDANFSe.ImprimeCanhoto then
      MontaCanhotoReciboTomador(3, 0);

    FinalizarArquivo;
  finally
    FFPDF.Free;
  end;


//  FPrintDialog := True;
//  FDetalharServico := False;


//  TfrlDANFSeRLRetrato.QuebradeLinha(TACBrNFSe(ACBrNFSe).Configuracoes.WebServices.QuebradeLinha);
//
//  if NFSe = nil then
//  begin
//    for i := 0 to TACBrNFSe(ACBrNFSe).NotasFiscais.Count - 1 do
//    begin
//      if Trim(self.NomeDocumento) <> ''  then
//        FPArquivoPDF := PathWithDelim(Self.PathPDF) + self.NomeDocumento + '-nfse.pdf'
//      else
//        FPArquivoPDF := PathWithDelim(Self.PathPDF) +
//          TACBrNFSe(ACBrNFSe).NumID[TACBrNFSe(ACBrNFSe).NotasFiscais.Items[i].NFSe] + '-nfse.pdf';
//
//      TfrlDANFSeRLRetrato.SalvarPDF(Self, TACBrNFSe(ACBrNFSe).NotasFiscais.Items[i].NFSe, FPArquivoPDF);
//    end;
//  end
//  else
//  begin
//    if Trim(self.NomeDocumento) <> ''  then
//      FPArquivoPDF := PathWithDelim(Self.PathPDF) + self.NomeDocumento + '-nfse.pdf'
//    else
//      FPArquivoPDF := PathWithDelim(Self.PathPDF) + TACBrNFSe(ACBrNFSe).NumID[NFSe] + '-nfse.pdf';
//
//    TfrlDANFSeRLRetrato.SalvarPDF(Self, NFSe, FPArquivoPDF);
//  end;
end;

procedure TACBrDANFSeFPDFA4Retrato.ImprimeCabecalhoDetalhesItensServico(const
    LargColunaDescricao, LargColunaValUnit, LargColunaQtde, LargColunaValServ,
    LargColunaBaseCalc, LargColunaISS: Double);
begin
  FFPDF.SetFont(IdentificadorTextoFonte, 'B', IdentificadorTextoTamanhoFonte);
  FFPDF.Cell(LargColunaDescricao, 4, SServTitColunaDescricao, 'LTB', 0, 'L');
  FFPDF.Cell(LargColunaValUnit,   4, SServTitColunaValorUnitario, 'TB', 0, 'R');
  FFPDF.Cell(LargColunaQtde,      4, SServTitColunaQuantidade, 'TB', 0, 'R');
  FFPDF.Cell(LargColunaValServ,   4, SServTitColunaValorServico, 'TB', 0, 'R');
  FFPDF.Cell(LargColunaBaseCalc,  4, SServTitColunaBaseCalculo, 'TB', 0, 'R');
  FFPDF.Cell(LargColunaISS,       4, SServTitColunaISS, 'TBR', 1, 'R');
end;

procedure TACBrDANFSeFPDFA4Retrato.ConfiguraDocumentoPDF;
begin
  // Propriedades do PDF
  FFPDF.SetUTF8(False);
  FFPDF.SetAliasNbPages();
  FFPDF.SetTitle('NFSe - ' + FNFSe.Numero);
//  FFPDF.SetAuthor('');
end;

procedure TACBrDANFSeFPDFA4Retrato.ConfiguraPagina;
var
  MargemEsq, MargemSup, MargemDir: Double;
begin
  FFPDF.SetCompression(True);
  //TACBrNFSeXDANFSeClass(FACBrNFSeXDANFSe)
  if fpDANFSe.MargemEsquerda > 0 then
    MargemEsq := fpDANFSe.MargemEsquerda
  else
    MargemEsq := 5;

  if fpDANFSe.MargemSuperior > 0 then
    MargemSup := fpDANFSe.MargemSuperior
  else
    MargemSup := 5;

  if fpDANFSe.MargemDireita > 0 then
    MargemDir := fpDANFSe.MargemDireita
  else
    MargemDir := 5;

  FFPDF.SetMargins(MargemEsq, MargemSup, MargemDir);

  FLarguraTabela := 190;
end;

procedure TACBrDANFSeFPDFA4Retrato.FinalizarArquivo;
//var
//  LSenhaPDF : String;
//  LPath : String;
//  LFile : String;
begin
//  LSenhaPDF := Trim(Self.PdfSenha);

//  if LSenhaPDF <> '' then
//    FFPDF.SetProtection([canPrint, canCopy],LSenhaPDF,'');

//  LPath := IncludeTrailingPathDelimiter(ExtractFilePath(Self.NomeArquivo));
//  LFile := ChangeFileExt(ExtractFileName(Self.NomeArquivo),'');

//  if LFile <> '' then
//    LFile := LFile+'_';

//  if Self.CalcularNomeArquivoPDFIndividual then
//    FFPDF.SaveToFile( ChangeFileExt(LPath + LFile+FNomeArquivo, '.pdf'))
//  else
    FFPDF.SaveToFile(FNomeArquivoPDFDestino);
end;

procedure TACBrDANFSeFPDFA4Retrato.ImprimeUnicoItem;
var
  TempStr: string;
begin
  FFPDF.SetFont(IdentificadorTextoFonte, '', IdentificadorTextoTamanhoFonte);
  TempStr := StringReplace(FNFSe.Servico.Discriminacao, FDadosAuxDANFSe.QuebradeLinha, #10, [rfReplaceAll, rfIgnoreCase]);
  FFPDF.MultiCell(FLarguraTabela, 4, TempStr, 'LTR', 'L');

  //ImprimirCodigoServico
  if (FNFSe.Servico.ItemListaServico <> '') or (FNFSe.Servico.xItemListaServico <> '')  then
  begin
    FFPDF.SetFont(IdentificadorTextoFonte, 'B', IdentificadorTextoTamanhoFonte);
    TempStr := SServCodigoServico + FNFSe.Servico.ItemListaServico +
            ' - ' + FNFSe.Servico.xItemListaServico;
    FFPDF.MultiCell(FLarguraTabela, 4, TempStr, 'LR', 'L');
  end;

  //Linha Fechando Quadro Item
  FFPDF.Cell(FLarguraTabela, 1, '', 'LRB', 1, 'C');
end;

procedure TACBrDANFSeFPDFA4Retrato.ImprimeVariosItens;
const
  CorDestaque = 26;//CorDeDestaque Cinza ~10% (26/255)
var
  LarguraColunaDescricao: Double;
  LarguraColunaValUnit: Double;
  LarguraColunaQtde: Double;
  LarguraColunaValServ: Double;
  LarguraColunaBaseCalc: Double;
  LarguraColunaISS: Double;
  ConfigDestacarLinha: Boolean;
  DestacarEssaLinha: Boolean;
  NumItem: Integer;
  TempPreY: Double;
  TempPreX: Double;
  TempPosY: Double;
  TempPosX: Double;
  TempStr: string;
begin
  LarguraColunaDescricao := 90;
  LarguraColunaValUnit   := 25;
  LarguraColunaQtde      := 15;
  LarguraColunaValServ   := 25;
  LarguraColunaBaseCalc  := 25;
  LarguraColunaISS       := 10;

  ImprimeCabecalhoDetalhesItensServico(LarguraColunaDescricao, LarguraColunaValUnit, LarguraColunaQtde,
      LarguraColunaValServ, LarguraColunaBaseCalc, LarguraColunaISS);

  FFPDF.SetFont(IdentificadorTextoFonte, '', IdentificadorTextoTamanhoFonte);
  ConfigDestacarLinha := False;
  FFPDF.SetFillColor(CorDestaque);
  for NumItem := 0 to fNFSe.Servico.ItemServico.Count-1 do
  begin
    DestacarEssaLinha := ConfigDestacarLinha and (NumItem mod 2 = 1);
    TempStr := StringReplace(FNFSe.Servico.ItemServico.Items[NumItem].Descricao, FDadosAuxDANFSe.QuebradeLinha, #10, [rfReplaceAll, rfIgnoreCase]);
    TempPreY := FFPDF.GetY;
    TempPreX := FFPDF.GetX;
    FFPDF.MultiCell(LarguraColunaDescricao, 4, TempStr, 'LT', 'L', DestacarEssaLinha);
    TempPosY := FFPDF.GetY;
    TempPosX := FFPDF.GetX;
    FFPDF.SetXY(TempPreX+LarguraColunaDescricao, TempPreY);
    //ImprimirDadosItem
    if fNFSe.Servico.ItemServico.Items[NumItem].ValorTotal = 0.0 then
      fNFSe.Servico.ItemServico.Items[NumItem].ValorTotal := fNFSe.Servico.ItemServico.Items[NumItem].Quantidade * fNFSe.Servico.ItemServico.Items[NumItem].ValorUnitario;
    if fNFSe.Servico.ItemServico.Items[NumItem].ValorISS = 0.0 then
      fNFSe.Servico.ItemServico.Items[NumItem].ValorISS := fNFSe.Servico.ItemServico.Items[NumItem].BaseCalculo * fNFSe.Servico.ItemServico.Items[NumItem].Aliquota/100;
    FFPDF.Cell(LarguraColunaValUnit,   4, FormatFloatBr(fNFSe.Servico.ItemServico.Items[NumItem].ValorUnitario), '', 0, 'R', DestacarEssaLinha);
    FFPDF.Cell(LarguraColunaQtde,      4, FormatFloatBr(fNFSe.Servico.ItemServico.Items[NumItem].Quantidade), '', 0, 'R', DestacarEssaLinha);
    FFPDF.Cell(LarguraColunaValServ,   4, FormatFloatBr(fNFSe.Servico.ItemServico.Items[NumItem].ValorTotal), '', 0, 'R', DestacarEssaLinha);
    FFPDF.Cell(LarguraColunaBaseCalc,  4, FormatFloatBr(fNFSe.Servico.ItemServico.Items[NumItem].BaseCalculo), '', 0, 'R', DestacarEssaLinha);
    FFPDF.Cell(LarguraColunaISS,       4, FormatFloatBr(fNFSe.Servico.ItemServico.Items[NumItem].ValorISS), '', 1, 'R', DestacarEssaLinha);
    FFPDF.SetXY(TempPosX, TempPosY);

    //ImprimirCodigoServico
    if (FNFSe.Servico.ItemServico.Items[NumItem].ItemListaServico <> '') or
       (FNFSe.Servico.ItemServico.Items[NumItem].xItemListaServico <> '')  then
    begin
      FFPDF.SetFont(IdentificadorTextoFonte, 'B', IdentificadorTextoTamanhoFonte);
      TempStr := SServCodigoServico + FNFSe.Servico.ItemServico.Items[NumItem].ItemListaServico +
              ' - ' + FNFSe.Servico.ItemServico.Items[NumItem].xItemListaServico;
      FFPDF.MultiCell(FLarguraTabela, 4, TempStr, 'LR', 'L', DestacarEssaLinha);
    end;

  end;

  //Linha Fechando Quadro Item
  FFPDF.Cell(FLarguraTabela, 1, '', 'LRB', 1, 'C');
end;

procedure TACBrDANFSeFPDFA4Retrato.MontaCabecalhoNFSe(const AEspacoAntes: Double; AEspacoDepois: Double);
var
  LArquivoLogoPrefeitura : string;
  LarguraLogo: Double;
  EspacoMargemTemp, TempY: Double;
  LarguraColuna1, LarguraColuna2: Double;
  TempStr: string;
begin
  LarguraLogo := 30;
  LarguraColuna2 := 37;
  LarguraColuna1 := FLarguraTabela - LarguraColuna2;
  if AEspacoAntes > 0 then
    FFPDF.Ln(AEspacoAntes);

  LArquivoLogoPrefeitura := fpDANFSe.Logo;
  if FileExists(LArquivoLogoPrefeitura) then
    FFPDF.Image(LArquivoLogoPrefeitura, FFPDF.GetX, FFPDF.GetY, LarguraLogo, LarguraLogo);

  TempY := FFPDF.GetY;
  FFPDF.Cell(LarguraLogo, 1, '', 'LT');
  FFPDF.SetFont(CabecalhoFonte, 'B', CabecalhoTamanhoFonte);
  FFPDF.MultiCell(LarguraColuna1 - LarguraLogo, 4, StringReplace(fpDANFSe.Prefeitura, FDadosAuxDANFSe.QuebradeLinha, #13#10, [rfReplaceAll, rfIgnoreCase]), 'T', 'C');

  //Volta a posição anterior na altura
  FFPDF.SetY(TempY);
  EspacoMargemTemp := LarguraColuna1;
  FFPDF.Cell(EspacoMargemTemp, 4, '', 'LR');
  FFPDF.SetFont(CabecalhoFonte, 'B', CabecalhoTamanhoFonte);
  FFPDF.Cell(LarguraColuna2, 4, SNumNota, 'LTR', 1, 'C');
  FFPDF.Cell(EspacoMargemTemp, 4, '', 'LR');
  FFPDF.SetFont(CabecalhoFonte, '', CabecalhoTamanhoFonte);
  FFPDF.Cell(LarguraColuna2, 4, FormatFloat('00000000000', StrToFloatDef(FNFSe.Numero, 0)), 'LR', 1, 'C');
  FFPDF.Cell(EspacoMargemTemp, 4, '', 'LR');
  FFPDF.SetFont(IdentificadorTextoFonte, 'B', IdentificadorTextoTamanhoFonte);
  FFPDF.Cell(LarguraColuna2, 4, SDataHoraEmissao, 'LTR', 1, 'C');
  FFPDF.Cell(EspacoMargemTemp, 4, '', 'LR');
  FFPDF.SetFont(IdentificadorTextoFonte, '', IdentificadorTextoTamanhoFonte);
  FFPDF.Cell(LarguraColuna2, 4, FormatDateTime('dd/mm/yyyy hh:nn', FNFSe.DataEmissao), 'LR', 1, 'C');
  FFPDF.Cell(EspacoMargemTemp, 4, '', 'LR');
  FFPDF.SetFont(IdentificadorTextoFonte, 'B', IdentificadorTextoTamanhoFonte);
  FFPDF.Cell(LarguraColuna2, 4, SNumNFSeSubstituida, 'LTR', 1, 'C');
  FFPDF.Cell(EspacoMargemTemp, 4, '', 'LR');
  FFPDF.SetFont(IdentificadorTextoFonte, '', IdentificadorTextoTamanhoFonte);
  FFPDF.Cell(LarguraColuna2, 4, FNFSe.NfseSubstituida, 'LR', 1, 'C');

  FFPDF.Cell(LarguraLogo, 6, '', 'L');
  FFPDF.SetFont(TituloFonte, 'B', TituloTamanhoFonte);
  FFPDF.Cell(LarguraColuna1 - LarguraLogo, 6, STituloNFSe, '', 0, 'C');
  FFPDF.Cell(LarguraColuna2, 6, '', 'LR', 1, 'C');

  //Linha Fecha QuadroSuperior
  FFPDF.Cell(EspacoMargemTemp, 1, '', 'LRB', 0);
  FFPDF.Cell(LarguraColuna2, 1, '', 'LRB', 1);

  FFPDF.SetFont(CabecalhoFonte, 'B', CabecalhoTamanhoFonte);
  FFPDF.Cell(25, 4, SCompetencia, 'LR', 0, 'L');
  FFPDF.SetFont(CabecalhoFonte, 'B', CabecalhoTamanhoFonte);
  FFPDF.Cell(25, 4, SNumRPS, 'LR', 0, 'L');
  FFPDF.SetFont(CabecalhoFonte, 'B', CabecalhoTamanhoFonte);
  FFPDF.Cell(60, 4, SMunicipioPrestacao,'LR', 0, 'L');
  FFPDF.SetFont(CabecalhoFonte, 'B', CabecalhoTamanhoFonte);
  FFPDF.Cell(60, 4, SCodigoVerificacao, 'LR', 0, 'L');
  FFPDF.SetFont(CabecalhoFonte, 'B', CabecalhoTamanhoFonte);
  FFPDF.Cell(20, 4, SPagina, 'LR', 1, 'L');

  FFPDF.SetFont(CabecalhoFonte, 'B', CabecalhoTamanhoFonte);
  FFPDF.Cell(25, 4, IfThen(FNFSe.Competencia > 0, FormatDateTime('mm/yyyy', FNFSe.Competencia), ''), 'LRB', 0, 'C');
  FFPDF.SetFont(CabecalhoFonte, 'B', CabecalhoTamanhoFonte);
  if FNFSe.IdentificacaoRps.Serie <> '' then
    TempStr := '/' + FNFSe.IdentificacaoRps.Serie
  else
    TempStr := '';
  FFPDF.Cell(25, 4, FNFSe.IdentificacaoRps.Numero+TempStr, 'LRB', 0, 'C');
  FFPDF.SetFont(CabecalhoFonte, 'B', CabecalhoTamanhoFonte);
  FFPDF.Cell(60, 4, FNFSe.Servico.MunicipioPrestacaoServico, 'LRB', 0, 'L');
  FFPDF.SetFont(CabecalhoFonte, 'B', CabecalhoTamanhoFonte);
  FFPDF.Cell(60, 4, FNFSe.CodigoVerificacao, 'LRB', 0, 'L');
  FFPDF.SetFont(CabecalhoFonte, 'B', CabecalhoTamanhoFonte);
  FFPDF.Cell(20, 4, IntToStr(FFPDF.PageNo)+'/{nb}', 'LRB', 1, 'L');

  if AEspacoDepois > 0 then
    FFPDF.Ln(AEspacoDepois);
end;

procedure TACBrDANFSeFPDFA4Retrato.MontaDadosPrestadorServico(
  const AEspacoAntes: Double; AEspacoDepois: Double);
var
  LCaminhoLogoPrestador: string;
  LarguraLogo: Integer;
  LPrestadorNomeRazao: string;
  EspacoMargemTemp, TamanhoStringTemp: Double;
  TempStr: string;
begin
  if AEspacoAntes > 0 then
    FFPDF.Ln(AEspacoAntes);

  FFPDF.SetFont(CabecalhoFonte, 'B', CabecalhoTamanhoFonte);
  FFPDF.Cell(190, 4, STituloPrestadorServicos, 'LTR', 1, 'C');

  LCaminhoLogoPrestador := fpDANFSe.Prestador.Logo;
  LarguraLogo := 30;
  if FileExists(LCaminhoLogoPrestador) then
    FFPDF.Image(LCaminhoLogoPrestador, FFPDF.GetX, FFPDF.GetY, LarguraLogo, LarguraLogo);

  EspacoMargemTemp := LarguraLogo+5;//larguralogo + borda
  FFPDF.Cell(EspacoMargemTemp);
  FFPDF.SetFont(IdentificadorTextoFonte, 'B', IdentificadorTextoTamanhoFonte);
  TamanhoStringTemp := FFPDF.GetStringWidth(SPrestadorNomeRazaoSocial);
  FFPDF.Cell(TamanhoStringTemp, 4, SPrestadorNomeRazaoSocial, '', 0, 'L');
  FFPDF.SetFont(IdentificadorTextoFonte, '', IdentificadorTextoTamanhoFonte);
  LPrestadorNomeRazao := IfThen(fNFSe.Prestador.RazaoSocial <> '',
                                fNFSe.Prestador.RazaoSocial, fpDANFSe.Prestador.RazaoSocial);
  if LPrestadorNomeRazao = '' then
    LPrestadorNomeRazao := IfThen(fNFSe.Prestador.NomeFantasia <> '',
                                  fNFSe.Prestador.NomeFantasia, fpDANFSe.Prestador.NomeFantasia);
  FFPDF.Cell(0, 4, LPrestadorNomeRazao , '', 1, 'L');

  FFPDF.Cell(EspacoMargemTemp);
  FFPDF.SetFont(IdentificadorTextoFonte, 'B', IdentificadorTextoTamanhoFonte);
  TamanhoStringTemp := FFPDF.GetStringWidth(SPrestadorCPFCNPJ);
  FFPDF.Cell(TamanhoStringTemp, 4, SPrestadorCPFCNPJ, '', 0, 'L');
  FFPDF.SetFont(IdentificadorTextoFonte, '', IdentificadorTextoTamanhoFonte);
  TempStr := FormatarCNPJouCPF(IfThen(FNFSe.Prestador.IdentificacaoPrestador.CpfCnpj <> '',
                                      FNFSe.Prestador.IdentificacaoPrestador.CpfCnpj,
                                      fpDANFSe.Prestador.CNPJ));
  FFPDF.Cell(190/2-TamanhoStringTemp-EspacoMargemTemp, 4, TempStr , '', 0, 'L');

  FFPDF.SetFont(IdentificadorTextoFonte, 'B', IdentificadorTextoTamanhoFonte);
  TamanhoStringTemp := FFPDF.GetStringWidth(SPrestadorInscMuni);
  FFPDF.Cell(TamanhoStringTemp, 4,  SPrestadorInscMuni, '', 0, 'L');
  FFPDF.SetFont(IdentificadorTextoFonte, '', IdentificadorTextoTamanhoFonte);
  TempStr := IfThen(FNFSe.Prestador.IdentificacaoPrestador.InscricaoMunicipal <> '',
                    FNFSe.Prestador.IdentificacaoPrestador.InscricaoMunicipal,
                    fpDANFSe.Prestador.InscricaoMunicipal);
  FFPDF.Cell(40, 4, TempStr , '', 0, 'L');

  FFPDF.SetFont(IdentificadorTextoFonte, 'B', IdentificadorTextoTamanhoFonte);
  TamanhoStringTemp := FFPDF.GetStringWidth(SPrestadorInscEst);
  FFPDF.Cell(TamanhoStringTemp, 4, SPrestadorInscEst, '', 0, 'L');
  FFPDF.SetFont(IdentificadorTextoFonte, '', IdentificadorTextoTamanhoFonte);
  TempStr := IfThen(FNFSe.Prestador.IdentificacaoPrestador.InscricaoEstadual <> '',
                    FNFSe.Prestador.IdentificacaoPrestador.InscricaoEstadual,
                    fpDANFSe.Prestador.InscricaoEstadual);
  FFPDF.Cell(0, 4, TempStr , '', 1, 'L');

  FFPDF.Cell(EspacoMargemTemp);
  FFPDF.SetFont(IdentificadorTextoFonte, '', IdentificadorTextoTamanhoFonte);
  TempStr := IfThen(FNFSe.Prestador.Endereco.Endereco <> '',
                    Trim(FNFSe.Prestador.Endereco.Endereco) + ', ' + Trim(Trim(FNFSe.Prestador.Endereco.Numero) + ' ' + Trim(IfThen(FNFSe.Prestador.Endereco.Complemento <> '', FNFSe.Prestador.Endereco.Complemento, fpDANFSe.Prestador.Complemento))) + #10 +
                      Trim(FNFSe.Prestador.Endereco.Bairro) + ' - ' + Trim(IfThen(FNFSe.Prestador.Endereco.xMunicipio <> '', FNFSe.Prestador.Endereco.xMunicipio, fpDANFSe.Prestador.Municipio)) + ' - ' +
                      IfThen(FNFSe.Prestador.Endereco.UF <> '', FNFSe.Prestador.Endereco.UF, fpDANFSe.Prestador.UF) + ' CEP: ' + FormatarCEP(FNFSe.Prestador.Endereco.CEP) + #10 +
                      Trim(IfThen(FNFSe.Prestador.Contato.Telefone <> '', FormatarFone(FNFSe.Prestador.Contato.Telefone), FormatarFone(fpDANFSe.Prestador.Fone)) + '  ' +
                      IfThen(FNFSe.Prestador.Contato.Email <> '', FNFSe.Prestador.Contato.Email, fpDANFSe.Prestador.Email)),
                    Trim(fpDANFSe.Prestador.Endereco));
  FFPDF.MultiCell(190-EspacoMargemTemp , 5, TempStr, '');

  //Linha Fechando Quadro
  FFPDF.Cell(190, 1, '', 'LRB', 1, 'C');

  if AEspacoDepois > 0 then
    FFPDF.Ln(AEspacoDepois);
end;

procedure TACBrDANFSeFPDFA4Retrato.MontaDadosTomadorServico(
  const AEspacoAntes: Double; AEspacoDepois: Double);
var
  EspacoMargemTemp, TamanhoStringTemp: Double;
  TempStr: string;
  UsaNifAoInvesDeCNPJ: Boolean;
begin
  if AEspacoAntes > 0 then
    FFPDF.Ln(AEspacoAntes);

  FFPDF.SetFont(CabecalhoFonte, 'B', CabecalhoTamanhoFonte);
  FFPDF.Cell(190, 4, STituloTomadorServicos, 'LTR', 1, 'C');

  EspacoMargemTemp := MargemInternaTabelas;// espaço da borda
  FFPDF.Cell(EspacoMargemTemp);
  FFPDF.SetFont(IdentificadorTextoFonte, 'B', IdentificadorTextoTamanhoFonte);
  TamanhoStringTemp := FFPDF.GetStringWidth(STomadorNomeRazaoSocial);
  FFPDF.Cell(TamanhoStringTemp, 4, STomadorNomeRazaoSocial, '', 0, 'L');
  FFPDF.SetFont(IdentificadorTextoFonte, '', IdentificadorTextoTamanhoFonte);
  FFPDF.Cell(0, 4, fNFSe.Tomador.RazaoSocial , '', 1, 'L');

  FFPDF.Cell(EspacoMargemTemp);
  FFPDF.SetFont(IdentificadorTextoFonte, 'B', IdentificadorTextoTamanhoFonte);
  UsaNifAoInvesDeCNPJ := (Length(FNFSe.Tomador.IdentificacaoTomador.Nif) > 0);
  TempStr := IfThen(UsaNifAoInvesDeCNPJ, STomadorNIF, STomadorCPFCNPJ);
  TamanhoStringTemp := FFPDF.GetStringWidth(TempStr);
  FFPDF.Cell(TamanhoStringTemp, 4, TempStr, '', 0, 'L');
  FFPDF.SetFont(IdentificadorTextoFonte, '', IdentificadorTextoTamanhoFonte);
  if UsaNifAoInvesDeCNPJ then
    TempStr := FNFSe.Tomador.IdentificacaoTomador.Nif
  else
    TempStr := FormatarCNPJouCPF(FNFSe.Tomador.IdentificacaoTomador.CpfCnpj);
  FFPDF.Cell(45, 4, TempStr , '', 0, 'L');

  FFPDF.SetFont(IdentificadorTextoFonte, 'B', IdentificadorTextoTamanhoFonte);
  TamanhoStringTemp := FFPDF.GetStringWidth(STomadorInscEst);
  FFPDF.Cell(TamanhoStringTemp, 4,  STomadorInscEst, '', 0, 'L');
  FFPDF.SetFont(IdentificadorTextoFonte, '', IdentificadorTextoTamanhoFonte);
  TempStr := IfThen(FNFSe.Tomador.IdentificacaoTomador.InscricaoEstadual <> '',
                    FNFSe.Tomador.IdentificacaoTomador.InscricaoEstadual,
                    fpDANFSe.Tomador.InscricaoMunicipal);
  FFPDF.Cell(40, 4, TempStr , '', 0, 'L');

  FFPDF.SetFont(IdentificadorTextoFonte, 'B', IdentificadorTextoTamanhoFonte);
  TamanhoStringTemp := FFPDF.GetStringWidth(STomadorInscMuni);
  FFPDF.Cell(TamanhoStringTemp, 4, STomadorInscMuni, '', 0, 'L');
  FFPDF.SetFont(IdentificadorTextoFonte, '', IdentificadorTextoTamanhoFonte);
  TempStr := IfThen(FNFSe.Tomador.IdentificacaoTomador.InscricaoEstadual <> '',
                    FNFSe.Tomador.IdentificacaoTomador.InscricaoEstadual,
                    fpDANFSe.Tomador.InscricaoEstadual);
  FFPDF.Cell(0, 4, TempStr , '', 1, 'L');

  FFPDF.Cell(EspacoMargemTemp);
  FFPDF.SetFont(IdentificadorTextoFonte, 'B', IdentificadorTextoTamanhoFonte);
  TamanhoStringTemp := FFPDF.GetStringWidth(STomadorEndereco);
  FFPDF.Cell(TamanhoStringTemp, 4, STomadorEndereco, '', 0, 'L');
  FFPDF.SetFont(IdentificadorTextoFonte, '', IdentificadorTextoTamanhoFonte);
  if fNFSe.Tomador.Endereco.Endereco <> '' then
  begin
    if fNFSe.Tomador.Endereco.UF = 'EX' then
    begin
      TempStr := Trim(fNFSe.Tomador.Endereco.Endereco) + ', Pais: ' + Trim(fNFSe.Tomador.Endereco.xPais);
    end
    else
      TempStr := Trim(fNFSe.Tomador.Endereco.Endereco) + ', ' +
        Trim(fNFSe.Tomador.Endereco.Numero) + ' - ' +
        Trim(fNFSe.Tomador.Endereco.Bairro) + ' - CEP: ' + FormatarCEP(fNFSe.Tomador.Endereco.CEP);
  end
  else
    TempStr := Trim(fpDANFSe.Tomador.Endereco) + ' - CEP: ' + FormatarCEP(fNFSe.Tomador.Endereco.CEP);
  FFPDF.Cell(0, 4, TempStr , '', 1, 'L');

  FFPDF.Cell(EspacoMargemTemp);
  FFPDF.SetFont(IdentificadorTextoFonte, 'B', IdentificadorTextoTamanhoFonte);
  TamanhoStringTemp := FFPDF.GetStringWidth(STomadorEnderecoComplemento);
  FFPDF.Cell(TamanhoStringTemp, 4, STomadorEnderecoComplemento, '', 0, 'L');
  FFPDF.SetFont(IdentificadorTextoFonte, '', IdentificadorTextoTamanhoFonte);
  FFPDF.Cell(190/2-TamanhoStringTemp, 4, IfThen(fNFSe.Tomador.Endereco.Complemento <> '',
                                                fNFSe.Tomador.Endereco.Complemento,
                                                fpDANFSe.Tomador.Complemento), '', 0, 'L');
  FFPDF.SetFont(IdentificadorTextoFonte, 'B', IdentificadorTextoTamanhoFonte);
  TamanhoStringTemp := FFPDF.GetStringWidth(STomadorTelefone);
  FFPDF.Cell(TamanhoStringTemp, 4, STomadorTelefone, '', 0, 'L');
  FFPDF.SetFont(IdentificadorTextoFonte, '', IdentificadorTextoTamanhoFonte);
  FFPDF.Cell(0, 4, FormatarFone(IfThen(fNFSe.Tomador.Contato.Telefone <> '',
                                       fNFSe.Tomador.Contato.Telefone,
                                       fpDANFSe.Tomador.Fone)
                               ), '', 1, 'L');

  FFPDF.Cell(EspacoMargemTemp);
  FFPDF.SetFont(IdentificadorTextoFonte, 'B', IdentificadorTextoTamanhoFonte);
  TamanhoStringTemp := FFPDF.GetStringWidth(STomadorMunicipio);
  FFPDF.Cell(TamanhoStringTemp, 4, STomadorMunicipio, '', 0, 'L');
  FFPDF.SetFont(IdentificadorTextoFonte, '', IdentificadorTextoTamanhoFonte);
  FFPDF.Cell(190/2-TamanhoStringTemp, 4, FNFSe.Tomador.Endereco.xMunicipio, '', 0, 'L');
  FFPDF.SetFont(IdentificadorTextoFonte, 'B', IdentificadorTextoTamanhoFonte);
  TamanhoStringTemp := FFPDF.GetStringWidth(STomadorUF);
  FFPDF.Cell(TamanhoStringTemp, 4, STomadorUF, '', 0, 'L');
  FFPDF.SetFont(IdentificadorTextoFonte, '', IdentificadorTextoTamanhoFonte);
  FFPDF.Cell(10, 4, FNFSe.Tomador.Endereco.UF, '', 0, 'L');
  FFPDF.SetFont(IdentificadorTextoFonte, 'B', IdentificadorTextoTamanhoFonte);
  TamanhoStringTemp := FFPDF.GetStringWidth(STomadorEmail);
  FFPDF.Cell(TamanhoStringTemp, 4, STomadorEmail, '', 0, 'L');
  FFPDF.SetFont(IdentificadorTextoFonte, '', IdentificadorTextoTamanhoFonte);
  FFPDF.Cell(0, 4, IfThen(FNFSe.Tomador.Contato.Email <> '', FNFSe.Tomador.Contato.Email,
                                                             fpDANFSe.Tomador.Email), '', 1, 'L');

  //Linha Fechando Quadro
  FFPDF.Cell(190, 1, '', 'LRB', 1, 'C');

  if AEspacoDepois > 0 then
    FFPDF.Ln(AEspacoDepois);
end;

procedure TACBrDANFSeFPDFA4Retrato.MontaMensagensMarcaDAgua(const AEspacoAntes: Double; AEspacoDepois: Double);
var
  ImprimirDestqueInvertido: Boolean;
begin
  ImprimirDestqueInvertido := True;

  if AEspacoAntes > 0 then
    FFPDF.Ln(AEspacoAntes);

  if ImprimirDestqueInvertido then
    FFPDF.SetTextColor(cWhite);

  if (fpDANFSe.Producao = snNao) then
  begin
    FFPDF.SetFont(CabecalhoFonte, 'B', CabecalhoTamanhoFonte);
    FFPDF.Cell(190, 4, SMsgHomologacaoSemValorFiscal, '1', 1, 'C', ImprimirDestqueInvertido);
  end;

  if fpDANFSe.Cancelada or fNFSe.ExibeComoCancelada then
  begin
    FFPDF.SetFont(CabecalhoFonte, 'B', CabecalhoTamanhoFonte);
    FFPDF.Cell(190, 4, SMsgNFSeCancelada, '1', 1, 'C', ImprimirDestqueInvertido);
  end;

  if ImprimirDestqueInvertido then
    FFPDF.SetTextColor(cBlack);

  if AEspacoDepois > 0 then
    FFPDF.Ln(AEspacoDepois);
end;

procedure TACBrDANFSeFPDFA4Retrato.MontaDadosDiscriminacaoServico(
  const AEspacoAntes: Double; AEspacoDepois: Double);
var
  TempStr: string;
begin
  if AEspacoAntes > 0 then
    FFPDF.Ln(AEspacoAntes);

  FFPDF.SetFont(CabecalhoFonte, 'B', CabecalhoTamanhoFonte);
  FFPDF.Cell(FLarguraTabela, 4, STituloDiscriminacaoServicos, '1', 1, 'C');

  //1 vs X itens
  if FDadosAuxDANFSe.Detalhar then
  begin
    ImprimeVariosItens;
  end
  else
  begin
    ImprimeUnicoItem;
  end;

  //ImprimirCodigoTributacaoMunicipio
  if (FNFSe.Servico.xCodigoTributacaoMunicipio <> '') then
  begin
    FFPDF.SetFont(IdentificadorTextoFonte, 'B', IdentificadorTextoTamanhoFonte);
    TempStr := SServCodigoTributacaoMunicipio + FNFSe.Servico.xCodigoTributacaoMunicipio;
    FFPDF.MultiCell(FLarguraTabela, 4, TempStr, '1', 'L');
  end;

  //  ImprimirAtividade
  if (fpDANFSe.Atividade <> '') then
  begin
    FFPDF.SetFont(IdentificadorTextoFonte, 'B', IdentificadorTextoTamanhoFonte);
    TempStr := SServAtividade + fpDANFSe.Atividade;
    FFPDF.MultiCell(FLarguraTabela, 4, TempStr, '1', 'L');
  end;

  if AEspacoDepois > 0 then
    FFPDF.Ln(AEspacoDepois);
end;

procedure TACBrDANFSeFPDFA4Retrato.MontaDetalhamentoEspecificoConstrucaoCivil(
  const AEspacoAntes: Double; AEspacoDepois: Double);

const
 SEspaco = '   ';
var
  TamanhoStringTemp: Double;
  LarguraUltimaColuna: Double;
begin
  LarguraUltimaColuna := FLarguraTabela;

  if AEspacoAntes > 0 then
    FFPDF.Ln(AEspacoAntes);


  FFPDF.SetFont(CabecalhoFonte, 'B', CabecalhoTamanhoFonte);
  TamanhoStringTemp := FFPDF.GetStringWidth(STituloConstrucaoCivil);
  FFPDF.Cell(TamanhoStringTemp, 4, STituloConstrucaoCivil, 'LTB', 0, 'L');
  LarguraUltimaColuna := LarguraUltimaColuna - TamanhoStringTemp;
  FFPDF.SetFont(IdentificadorTextoFonte, '', IdentificadorTextoTamanhoFonte);
  TamanhoStringTemp := FFPDF.GetStringWidth(SEspaco);
  FFPDF.Cell(TamanhoStringTemp, 4, SEspaco, 'TB', 0, 'L');
  LarguraUltimaColuna := LarguraUltimaColuna - TamanhoStringTemp;
  FFPDF.SetFont(IdentificadorTextoFonte, 'B', IdentificadorTextoTamanhoFonte);
  TamanhoStringTemp := FFPDF.GetStringWidth(SConstrCivilCodObra);
  FFPDF.Cell(TamanhoStringTemp, 4, SConstrCivilCodObra, 'TB', 0, 'L');
  LarguraUltimaColuna := LarguraUltimaColuna - TamanhoStringTemp;
  FFPDF.SetFont(IdentificadorTextoFonte, '', IdentificadorTextoTamanhoFonte);
  TamanhoStringTemp := FFPDF.GetStringWidth(FNFSe.ConstrucaoCivil.CodigoObra);
  FFPDF.Cell(TamanhoStringTemp, 4, FNFSe.ConstrucaoCivil.CodigoObra, 'TB', 0, 'L');
  LarguraUltimaColuna := LarguraUltimaColuna - TamanhoStringTemp;
  FFPDF.SetFont(IdentificadorTextoFonte, '', IdentificadorTextoTamanhoFonte);
  TamanhoStringTemp := FFPDF.GetStringWidth(SEspaco);
  FFPDF.Cell(TamanhoStringTemp, 4, SEspaco, 'TB', 0, 'L');
  LarguraUltimaColuna := LarguraUltimaColuna - TamanhoStringTemp;
  FFPDF.SetFont(IdentificadorTextoFonte, 'B', IdentificadorTextoTamanhoFonte);
  TamanhoStringTemp := FFPDF.GetStringWidth(SConstrCivilCodART);
  FFPDF.Cell(TamanhoStringTemp, 4, SConstrCivilCodART, 'TB', 0, 'L');
  LarguraUltimaColuna := LarguraUltimaColuna - TamanhoStringTemp;
  FFPDF.SetFont(IdentificadorTextoFonte, '', IdentificadorTextoTamanhoFonte);
//  TamanhoStringTemp := FFPDF.GetStringWidth(FNFSe.ConstrucaoCivil.Art);
  FFPDF.Cell(LarguraUltimaColuna, 4, FNFSe.ConstrucaoCivil.Art, 'TRB', 0, 'L');
  FFPDF.Ln();

  if AEspacoDepois > 0 then
    FFPDF.Ln(AEspacoDepois);
end;

procedure TACBrDANFSeFPDFA4Retrato.MontaDadosImpostosETotais(
  const AEspacoAntes: Double; AEspacoDepois: Double);
var
  TamanhoStringTemp: Double;
  TempStr: string;
  TamanhoColuna1, TamanhoColuna2, TamanhoColuna3: Double;
  TamanhoColuna1A, TamanhoColuna1B, TamanhoColuna3A, TamanhoColuna3B: Double;
  TempX, TempY: Double;
begin
  TamanhoColuna2 := FLarguraTabela * 0.23;
  TamanhoColuna1 := (FLarguraTabela - TamanhoColuna2)/2;
  TamanhoColuna3 := FLarguraTabela - TamanhoColuna1- TamanhoColuna2;

  TamanhoColuna1A := TamanhoColuna1/2;
  TamanhoColuna1B := TamanhoColuna1-TamanhoColuna1A;
  TamanhoColuna3A := TamanhoColuna3/2;
  TamanhoColuna3B := TamanhoColuna3-TamanhoColuna3A;

  if AEspacoAntes > 0 then
    FFPDF.Ln(AEspacoAntes);

  FFPDF.SetFont(CabecalhoFonte, 'B', CabecalhoTamanhoFonte);
  FFPDF.Cell(FLarguraTabela, 4, STituloTributosFederais, '1', 1, 'C');

  FFPDF.SetFont(IdentificadorTextoFonte, 'B', IdentificadorTextoTamanhoFonte);
  TamanhoStringTemp := FFPDF.GetStringWidth(STribFedPIS);
  FFPDF.Cell(TamanhoStringTemp, 4, STribFedPIS, 'L', 0, 'L');
  FFPDF.SetFont(IdentificadorTextoFonte, '', IdentificadorTextoTamanhoFonte);
  FFPDF.Cell(TamanhoColuna1A-TamanhoStringTemp, 4, FormatFloat(',0.00', FNFSe.Servico.Valores.ValorPis), 'R', 0, 'R');

  FFPDF.SetFont(IdentificadorTextoFonte, 'B', IdentificadorTextoTamanhoFonte);
  TamanhoStringTemp := FFPDF.GetStringWidth(STribFedCOFINS);
  FFPDF.Cell(TamanhoStringTemp, 4, STribFedCOFINS, 'L', 0, 'L');
  FFPDF.SetFont(IdentificadorTextoFonte, '', IdentificadorTextoTamanhoFonte);
  FFPDF.Cell(TamanhoColuna1B-TamanhoStringTemp, 4, FormatFloat(',0.00', FNFSe.Servico.Valores.ValorCofins), 'R', 0, 'R');

  FFPDF.SetFont(IdentificadorTextoFonte, 'B', IdentificadorTextoTamanhoFonte);
  TamanhoStringTemp := FFPDF.GetStringWidth(STribFedIR);
  FFPDF.Cell(TamanhoStringTemp, 4, STribFedIR, 'L', 0, 'L');
  FFPDF.SetFont(IdentificadorTextoFonte, '', IdentificadorTextoTamanhoFonte);
  FFPDF.Cell(TamanhoColuna2-TamanhoStringTemp, 4, FormatFloat(',0.00', FNFSe.Servico.Valores.ValorIr), 'R', 0, 'R');

  FFPDF.SetFont(IdentificadorTextoFonte, 'B', IdentificadorTextoTamanhoFonte);
  TamanhoStringTemp := FFPDF.GetStringWidth(STribFedINSS);
  FFPDF.Cell(TamanhoStringTemp, 4, STribFedINSS, 'L', 0, 'L');
  FFPDF.SetFont(IdentificadorTextoFonte, '', IdentificadorTextoTamanhoFonte);
  FFPDF.Cell(TamanhoColuna3A-TamanhoStringTemp, 4, FormatFloat(',0.00', FNFSe.Servico.Valores.ValorInss), 'R', 0, 'R');

  FFPDF.SetFont(IdentificadorTextoFonte, 'B', IdentificadorTextoTamanhoFonte);
  TamanhoStringTemp := FFPDF.GetStringWidth(STribFedCSLL);
  FFPDF.Cell(TamanhoStringTemp, 4, STribFedCSLL, 'L', 0, 'L');
  FFPDF.SetFont(IdentificadorTextoFonte, '', IdentificadorTextoTamanhoFonte);
  FFPDF.Cell(TamanhoColuna3B-TamanhoStringTemp, 4, FormatFloat(',0.00', FNFSe.Servico.Valores.ValorCsll), 'R', 0, 'R');
  FFPDF.Ln();

  FFPDF.SetFont(IdentificadorTextoFonte, 'B', IdentificadorTextoTamanhoFonte);
  FFPDF.Cell(TamanhoColuna1, 4, SCabecalhoTribDetalhamentoValores, 'LTR', 0, 'C');
  FFPDF.SetFont(IdentificadorTextoFonte, 'B', IdentificadorTextoTamanhoFonte);
  FFPDF.Cell(TamanhoColuna2, 4, SCabecalhoTribOutrasInformacoes, 'LTR', 0, 'C');
  FFPDF.SetFont(IdentificadorTextoFonte, 'B', IdentificadorTextoTamanhoFonte);
  FFPDF.Cell(TamanhoColuna1, 4, SCabecalhoTribCalculoISSQNDevido, 'LTR', 0, 'C');
  FFPDF.Ln();

  FFPDF.SetFont(IdentificadorTextoFonte, '', IdentificadorTextoTamanhoFonte);
  TamanhoStringTemp := FFPDF.GetStringWidth(STribDetVal_ValorServicos);
  FFPDF.Cell(TamanhoStringTemp, 4, STribDetVal_ValorServicos, 'L', 0, 'L');
  FFPDF.Cell(TamanhoColuna1-TamanhoStringTemp, 4, FormatFloat(',0.00', FNFSe.Servico.Valores.ValorServicos), 'R', 0, 'R');
  FFPDF.SetFont(IdentificadorTextoFonte, '', IdentificadorTextoTamanhoFonte);
  FFPDF.Cell(TamanhoColuna2, 4, IfThen(FDadosAuxDANFSe.NaturezaOperacaoDescricao <> '', STribOutrInf_NaturezaOperacao, ''), 'LR', 0, 'C');
  FFPDF.SetFont(IdentificadorTextoFonte, '', IdentificadorTextoTamanhoFonte);
  TamanhoStringTemp := FFPDF.GetStringWidth(STribCalcISSQN_ValorServicos);
  FFPDF.Cell(TamanhoStringTemp, 4, STribCalcISSQN_ValorServicos, 'L', 0, 'L');
  FFPDF.Cell(TamanhoColuna3-TamanhoStringTemp, 4, FormatFloat(',0.00', FNFSe.Servico.Valores.ValorServicos), 'R', 0, 'R');
  FFPDF.Ln();

  FFPDF.SetFont(IdentificadorTextoFonte, '', IdentificadorTextoTamanhoFonte);
  TamanhoStringTemp := FFPDF.GetStringWidth(STribDetVal_DescontoIncondicionado);
  FFPDF.Cell(TamanhoStringTemp, 4, STribDetVal_DescontoIncondicionado, 'L', 0, 'L');
  FFPDF.Cell(TamanhoColuna1-TamanhoStringTemp, 4, FormatFloat(',0.00', FNFSe.Servico.Valores.DescontoIncondicionado), 'R', 0, 'R');
  FFPDF.SetFont(IdentificadorTextoFonte, '', IdentificadorTextoTamanhoFonte);
  FFPDF.Cell(TamanhoColuna2, 4, FDadosAuxDANFSe.NaturezaOperacaoDescricao, 'LR', 0, 'C');
  FFPDF.SetFont(IdentificadorTextoFonte, '', IdentificadorTextoTamanhoFonte);
  TamanhoStringTemp := FFPDF.GetStringWidth(STribCalcISSQN_DeducoesPermitidas);
  FFPDF.Cell(TamanhoStringTemp, 4, STribCalcISSQN_DeducoesPermitidas, 'L', 0, 'L');
  FFPDF.Cell(TamanhoColuna3-TamanhoStringTemp, 4, FormatFloat(',0.00', FNFSe.Servico.Valores.ValorDeducoes), 'R', 0, 'R');
  FFPDF.Ln();

  FFPDF.SetFont(IdentificadorTextoFonte, '', IdentificadorTextoTamanhoFonte);
  TamanhoStringTemp := FFPDF.GetStringWidth(STribDetVal_DescontoCondicionado);
  FFPDF.Cell(TamanhoStringTemp, 4, STribDetVal_DescontoCondicionado, 'L', 0, 'L');
  FFPDF.Cell(TamanhoColuna1-TamanhoStringTemp, 4, FormatFloat(',0.00', FNFSe.Servico.Valores.DescontoCondicionado), 'R', 0, 'R');
  FFPDF.SetFont(IdentificadorTextoFonte, '', IdentificadorTextoTamanhoFonte);
  FFPDF.Cell(TamanhoColuna2, 4, '', 'LR', 0, 'C');
  FFPDF.SetFont(IdentificadorTextoFonte, '', IdentificadorTextoTamanhoFonte);
  TamanhoStringTemp := FFPDF.GetStringWidth(STribCalcISSQN_DescontoIncondicionado);
  FFPDF.Cell(TamanhoStringTemp, 4, STribCalcISSQN_DescontoIncondicionado, 'L', 0, 'L');
  FFPDF.Cell(TamanhoColuna3-TamanhoStringTemp, 4, FormatFloat(',0.00', FNFSe.Servico.Valores.DescontoIncondicionado), 'R', 0, 'R');
  FFPDF.Ln();

  FFPDF.SetFont(IdentificadorTextoFonte, '', IdentificadorTextoTamanhoFonte);
  TamanhoStringTemp := FFPDF.GetStringWidth(STribDetVal_RetencoesFederais);
  FFPDF.Cell(TamanhoStringTemp, 4, STribDetVal_RetencoesFederais, 'L', 0, 'L');
  FFPDF.Cell(TamanhoColuna1-TamanhoStringTemp, 4, FormatFloat(',0.00', FNFSe.Servico.Valores.RetencoesFederais), 'R', 0, 'R');
  FFPDF.SetFont(IdentificadorTextoFonte, '', IdentificadorTextoTamanhoFonte);
  FFPDF.Cell(TamanhoColuna2, 4, STribOutrInf_RegimeEspecial, 'LR', 0, 'C');
  FFPDF.SetFont(IdentificadorTextoFonte, '', IdentificadorTextoTamanhoFonte);
  TamanhoStringTemp := FFPDF.GetStringWidth(STribCalcISSQN_BaseCalculo);
  FFPDF.Cell(TamanhoStringTemp, 4, STribCalcISSQN_BaseCalculo, 'L', 0, 'L');
  FFPDF.Cell(TamanhoColuna3-TamanhoStringTemp, 4, FormatFloat(',0.00', FNFSe.Servico.Valores.BaseCalculo), 'R', 0, 'R');
  FFPDF.Ln();

  FFPDF.SetFont(IdentificadorTextoFonte, '', IdentificadorTextoTamanhoFonte);
  TamanhoStringTemp := FFPDF.GetStringWidth(STribDetVal_OutrasRetencoes);
  FFPDF.Cell(TamanhoStringTemp, 4, STribDetVal_OutrasRetencoes, 'L', 0, 'L');
  FFPDF.Cell(TamanhoColuna1-TamanhoStringTemp, 4, FormatFloat(',0.00', FNFSe.Servico.Valores.OutrasRetencoes), 'R', 0, 'R');
  FFPDF.SetFont(IdentificadorTextoFonte, '', IdentificadorTextoTamanhoFonte);
//  FFPDF.Cell(TamanhoColuna2, 4, FRegimeEspecial, 'LR', 0, 'C');
  TempX := FFPDF.GetX;
  TempY := FFPDF.GetY;
  FFPDF.MultiCell(TamanhoColuna2, 4, FDadosAuxDANFSe.RegimeEspecialDescricao, 'LR', 'C');
  FFPDF.SetXY(TempX+TamanhoColuna2, TempY);
  FFPDF.SetFont(IdentificadorTextoFonte, '', IdentificadorTextoTamanhoFonte);
  TamanhoStringTemp := FFPDF.GetStringWidth(STribCalcISSQN_Aliquota);
  FFPDF.Cell(TamanhoStringTemp, 4, STribCalcISSQN_Aliquota, 'L', 0, 'L');
  FFPDF.Cell(TamanhoColuna3-TamanhoStringTemp, 4, FormatFloat(',0.00', FNFSe.Servico.Valores.Aliquota), 'R', 0, 'R');
  FFPDF.Ln();

  FFPDF.SetFont(IdentificadorTextoFonte, '', IdentificadorTextoTamanhoFonte);
  TamanhoStringTemp := FFPDF.GetStringWidth(STribDetVal_ISSRetido);
  FFPDF.Cell(TamanhoStringTemp, 4, STribDetVal_ISSRetido, 'L', 0, 'L');
  FFPDF.Cell(TamanhoColuna1-TamanhoStringTemp, 4, FormatFloat(',0.00', FNFSe.Servico.Valores.ValorIssRetido), 'R', 0, 'R');
  FFPDF.SetFont(IdentificadorTextoFonte, '', IdentificadorTextoTamanhoFonte);
  FFPDF.Cell(TamanhoColuna2, 4, '', 'LR', 0, 'C');
  FFPDF.SetFont(IdentificadorTextoFonte, '', IdentificadorTextoTamanhoFonte);
  TamanhoStringTemp := FFPDF.GetStringWidth(STribCalcISSQN_ISSAReter);
  FFPDF.Cell(TamanhoStringTemp, 4, STribCalcISSQN_ISSAReter, 'L', 0, 'L');
  FFPDF.Cell(TamanhoColuna3-TamanhoStringTemp, 4, FDadosAuxDANFSe.IssAReterDescricao, 'R', 0, 'R');
  FFPDF.Ln();
//
  FFPDF.SetFont(IdentificadorTextoFonte, 'B', IdentificadorTextoTamanhoFonte);
  TamanhoStringTemp := FFPDF.GetStringWidth(STribDetVal_ValorLiquido);
  FFPDF.Cell(TamanhoStringTemp, 4, STribDetVal_ValorLiquido, 'TL', 0, 'L');
  FFPDF.Cell(TamanhoColuna1-TamanhoStringTemp, 4, FormatFloat(',0.00', FNFSe.Servico.Valores.ValorLiquidoNfse), 'TR', 0, 'R');
  FFPDF.SetFont(IdentificadorTextoFonte, '', IdentificadorTextoTamanhoFonte);
  FFPDF.Cell(TamanhoColuna2, 4, STribOutrInf_OpcaoSimplesNacional + FDadosAuxDANFSe.OptanteSimplesDescricao, 'LTR', 0, 'C');
  FFPDF.SetFont(IdentificadorTextoFonte, 'B', IdentificadorTextoTamanhoFonte);
  TamanhoStringTemp := FFPDF.GetStringWidth(STribCalcISSQN_ValorISS);
  FFPDF.Cell(TamanhoStringTemp, 4, STribCalcISSQN_ValorISS, 'T', 0, 'L');
  FFPDF.Cell(TamanhoColuna3-TamanhoStringTemp, 4, FormatFloat(',0.00', FNFSe.Servico.Valores.ValorIss), 'TR', 0, 'R');
  FFPDF.Ln();

  FFPDF.SetFont(IdentificadorTextoFonte, '', IdentificadorTextoTamanhoFonte);
  FFPDF.Cell(TamanhoColuna1, 4, '', 'LRB');
  FFPDF.Cell(TamanhoColuna2, 4, STribOutrInf_IncentivadorCultural + FDadosAuxDANFSe.IncentivadorCulturalDescricao, 'LRB', 0, 'C');
  FFPDF.Cell(TamanhoColuna3, 4, '', 'LRB');
  FFPDF.Ln();

//Total Nota
  FFPDF.SetFont(CabecalhoFonte, 'B', CabecalhoTamanhoFonte);
  TempStr := STribValorTotalNota + FormatFloat(',0.00', FNFSe.Servico.Valores.ValorServicos);
  FFPDF.Cell(FLarguraTabela, 4, TempStr, '1', 1, 'C');

  if AEspacoDepois > 0 then
    FFPDF.Ln(AEspacoDepois);

end;

procedure TACBrDANFSeFPDFA4Retrato.MontaOutrasInformacoes(
  const AEspacoAntes: Double; AEspacoDepois: Double);
const
  TamanhoQrCode: Double = 25;
  TamahoMinQuadroSemQrCode: Double = 10;
var
  TamanhoStringTemp: Double;
  TempStr: string;
  TemQrCode: Boolean;
  PosXInicioQrCode, PosYInicioQuadroOutrInfo: Double;
  TamanhoMinimoQuadro: Double;
begin
  if AEspacoAntes > 0 then
    FFPDF.Ln(AEspacoAntes);

  TemQrCode := (FNFSe.Link <> '');
  PosYInicioQuadroOutrInfo := FFPDF.GetY;

  FFPDF.SetFont(CabecalhoFonte, 'B', CabecalhoTamanhoFonte);
  FFPDF.Cell(190, 4, STituloOutrasInformacoes, 'LTR', 1, 'C');

  if TemQrCode then
  begin
    //ImprimeQRCode
    PosXInicioQrCode := FFPDF.GetX + 190-TamanhoQrCode;
    TamanhoMinimoQuadro := TamanhoQrCode;
    FFPDF.QRCode(PosXInicioQrCode, FFPDF.GetY-4, FNFSe.Link, 0.6);
  end
  else
  begin
    PosXInicioQrCode := 0;
    TamanhoMinimoQuadro := TamahoMinQuadroSemQrCode;
  end;

  FFPDF.Cell(MargemInternaTabelas);
  FFPDF.SetFont(TextoFonte, '', TextoTamanhoFonte);
  TempStr := '';
  if fpDANFSe.OutrasInformacaoesImp <> '' then
    TempStr := fpDANFSe.OutrasInformacaoesImp
  else
    if fNFSe.OutrasInformacoes <> '' then
      TempStr := fNFSe.OutrasInformacoes;
  if fNFSe.InformacoesComplementares <> '' then
  begin
    if TempStr <> '' then TempStr := TempStr + #10;
    TempStr := fNFSe.InformacoesComplementares;
  end;
  TempStr := StringReplace(TempStr, FDadosAuxDANFSe.QuebradeLinha, #10, [rfReplaceAll, rfIgnoreCase]);
  TamanhoStringTemp := PosXInicioQrCode - FFPDF.GetX;
  FFPDF.MultiCell(TamanhoStringTemp, 4, TempStr, '', 'L');
  if (FFPDF.GetY - PosYInicioQuadroOutrInfo) <= TamanhoMinimoQuadro then
    FFPDF.Ln(TamanhoMinimoQuadro - (FFPDF.GetY - PosYInicioQuadroOutrInfo));

  //Linha Fechando Quadro
  FFPDF.Cell(190, 1, '', 'LRB', 1, 'C');

  // Imprimir DataHora da Impressão, Usuário e Sistema
  FFPDF.SetFont(TextoPequenoFonte, '', TextoPequenoTamanhoFonte);
  TempStr :=  sOutrasInfDataHora +' '+ FormatDateTime('dd/mm/yyyy hh:nn', Now);
  if fpDANFSe.Usuario <> '' then
    TempStr := TempStr + sOutrasInfUsuario+'   ' +fpDANFSe.Usuario;
  FFPDF.Cell(190/2, 4, TempStr, '', 0, 'L');
  if fpDANFSe.Sistema <> '' then
  begin
    TempStr := sOutrasInfDesenvolvidoPor+' '+fpDANFSe.Sistema;
    FFPDF.Cell(190/2 - 1, 4, TempStr, '', 0, 'R');
  end;
  FFPDF.Ln();

  if AEspacoDepois > 0 then
    FFPDF.Ln(AEspacoDepois);
end;

procedure TACBrDANFSeFPDFA4Retrato.MontaCanhotoReciboTomador(
  const AEspacoAntes: Double; AEspacoDepois: Double);
var
  EspacoMargemTemp, TamanhoStringTemp: Double;
  TamanhoSegundaColuna: Double;
  TempStr: string;
begin
  TamanhoSegundaColuna := 30;

  if AEspacoAntes > 0 then
    FFPDF.Ln(AEspacoAntes);

  EspacoMargemTemp := MargemInternaTabelas;// espaço da borda
  FFPDF.SetFont(IdentificadorTextoFonte, '', IdentificadorTextoTamanhoFonte);
  FFPDF.Cell(EspacoMargemTemp, 4, '', 'LT');
  TamanhoStringTemp := FFPDF.GetStringWidth(SCanhotoRecibo_Recebemos);
  FFPDF.Cell(TamanhoStringTemp, 4, SCanhotoRecibo_Recebemos, 'T', 0, 'L');
  FFPDF.SetFont(IdentificadorTextoFonte, 'B', IdentificadorTextoTamanhoFonte);
  TamanhoStringTemp := 190-EspacoMargemTemp-TamanhoStringTemp-TamanhoSegundaColuna;
  FFPDF.Cell(TamanhoStringTemp, 4,
             IfThen(fNFSe.Prestador.RazaoSocial <> '',
                    fNFSe.Prestador.RazaoSocial,
                    fpDANFSe.Prestador.RazaoSocial),
             'TR', 0, 'C');

  FFPDF.SetFont(IdentificadorTextoFonte, 'B', IdentificadorTextoTamanhoFonte);
  FFPDF.Cell(TamanhoSegundaColuna, 4, SCanhotoRecibo_NumeroNota, 'LTR', 1, 'C');

  FFPDF.SetFont(IdentificadorTextoFonte, '', IdentificadorTextoTamanhoFonte);
  FFPDF.Cell(EspacoMargemTemp, 4, '','L');
//  TamanhoStringTemp := FFPDF.GetStringWidth(SCanhotoRecibo_OsServicosConstantes);
  TamanhoStringTemp := 190-EspacoMargemTemp-TamanhoSegundaColuna;
  FFPDF.Cell(TamanhoStringTemp, 4, SCanhotoRecibo_OsServicosConstantes, '', 0, 'L');
  FFPDF.SetFont(IdentificadorTextoFonte, 'B', IdentificadorTextoTamanhoFonte);
  FFPDF.Cell(TamanhoSegundaColuna, 4, FormatFloat('00000000000', StrToFloatDef(fNFSe.Numero, 0)), 'LR', 1, 'C');

  FFPDF.Cell(EspacoMargemTemp, 4, '','L');
  TempStr := SCanhotoRecibo_Emissao + FormatDateTime('dd/mm/yy', fNFSe.DataEmissao) +
             SCanhotoRecibo_Tomador + fNFSe.Tomador.RazaoSocial +
             SCanhotoRecibo_Total + FormatFloat(',0.00', fNFSe.Servico.Valores.ValorLiquidoNfse);
  FFPDF.SetFont(IdentificadorTextoFonte, '', IdentificadorTextoTamanhoFonte);
  TamanhoStringTemp := 190-EspacoMargemTemp-TamanhoSegundaColuna;
  FFPDF.Cell(TamanhoStringTemp, 4, TempStr, '', 0, 'L');
  FFPDF.SetFont(IdentificadorTextoFonte, 'B', IdentificadorTextoTamanhoFonte);
  FFPDF.Cell(TamanhoSegundaColuna, 4, '', 'LR', 1, 'C');

  FFPDF.Cell(EspacoMargemTemp, 4, '','L');
  TempStr := SCanhotoRecibo_Data + SCanhotoRecibo_DataPlaceHolder + '  ' +
             SCanhotoRecibo_IntentificacaoAssinatura;
  FFPDF.SetFont(IdentificadorTextoFonte, '', IdentificadorTextoTamanhoFonte);
  TamanhoStringTemp := 190-EspacoMargemTemp-TamanhoSegundaColuna;
  FFPDF.Cell(TamanhoStringTemp, 4, TempStr, '', 0, 'L');
  FFPDF.SetFont(IdentificadorTextoFonte, 'B', IdentificadorTextoTamanhoFonte);
  FFPDF.Cell(TamanhoSegundaColuna, 4, '', 'LR', 1, 'C');

  TamanhoStringTemp := 190-TamanhoSegundaColuna;
  FFPDF.Cell(TamanhoStringTemp, 1, '','LBR', 0);
  FFPDF.Cell(TamanhoSegundaColuna, 1, '','LBR', 1);

  if AEspacoDepois > 0 then
    FFPDF.Ln(AEspacoDepois);
end;

end.
