{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
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

unit ACBrPAFNFCe_MenuFiscal;

interface

uses
  ACBrPAFNFCe_Comum,
  pcnSignature,
  Classes,
  SysUtils,
  Forms;

type
  TACBrPAFNFCe_MenuFiscal = class(TACBrPAFNFCe_BaseFile)
  private
    FArquivo: WideString;
    FArquivoBase64: WideString;
    FNumeroArquivo: TACBrPAFNFCe_NumeroArquivo;
    FDataGeracaoArquivo: TDateTime;
    FHoraGeracaoArquivo: TDateTime;
    FArquiteturaBD: TACBrPAFNFCe_ArquiteturaBD;
    FArquiteturaSistema: TACBrPAFNFCe_ArquiteturaSistema;
    FSignature: TSignature;
    procedure GerarXMLMenuFiscal(Assinado: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Inicializar;
    procedure GerarXML(const Assinar: Boolean = True); override;
    procedure LoadFromFile(const AFileName: String); override;
    procedure SaveToFile(const AXmlFileName: String; const AAssinar: Boolean = True); override;

    property Arquivo: WideString read FArquivo write FArquivo;
    property ArquivoBase64: WideString read FArquivoBase64 write FArquivoBase64;
    property NumeroArquivo: TACBrPAFNFCe_NumeroArquivo read FNumeroArquivo write FNumeroArquivo;
    property DataGeracaoArquivo: TDateTime read FDataGeracaoArquivo write FDataGeracaoArquivo;
    property HoraGeracaoArquivo: TDateTime read FHoraGeracaoArquivo write FHoraGeracaoArquivo;
    property ArquiteturaBD: TACBrPAFNFCe_ArquiteturaBD read FArquiteturaBD write FArquiteturaBD;
    property ArquiteturaSistema: TACBrPAFNFCe_ArquiteturaSistema read FArquiteturaSistema write FArquiteturaSistema;
  end;

implementation

uses
  ACBrDFeConsts,
  pcnConversao,
  pcnGerador,
  synacode,
  ACBrUtil.Strings,
  ACBrUtil.XMLHTML,
  ACBrUtil.FilesIO,
  ACBrPAFNFCe,
  pcnAuxiliar,
  pcnLeitor;


{ TACBrPAFNFCe_MenuFiscal }

constructor TACBrPAFNFCe_MenuFiscal.Create(AOwner: TComponent);
begin
  inherited;
  Inicializar;
end;

destructor TACBrPAFNFCe_MenuFiscal.Destroy;
begin
  FSignature.Free;
  inherited;
end;

procedure TACBrPAFNFCe_MenuFiscal.GerarXMLMenuFiscal(Assinado: Boolean);
var
  I, X: Integer;
  sArquivoBase64: WideString;
  sAtributo: String;

  procedure AtributoAdd(Atributo, Valor: String);
  begin
    if (sAtributo <> '') then
      sAtributo := sAtributo + ' ';
    sAtributo := sAtributo + Atributo + '="' + Valor + '"';
  end;

begin
  FXMLOriginal := '';
  FXMLAssinado := '';
  FGerador.ArquivoFormatoXML := '';

  FGerador.wGrupo(ENCODING_UTF8, '', False);
  FGerador.wGrupo('menuFiscal xmlns="http://www.sef.sc.gov.br/nfce"');

  sAtributo := '';
  if (NumeroArquivo <> tnroArq_None) then
    AtributoAdd('nroArquivo',NumeroArquivoToStr(NumeroArquivo));
  AtributoAdd('data',FORMATDATETIME('ddmmyyyy',DataGeracaoArquivo));
  AtributoAdd('hora',FORMATDATETIME('hhmmss',HoraGeracaoArquivo));
  if (ArquiteturaBD <> tarqBD_None) then
    AtributoAdd('arqBD',ArquiteturaBDToStr(ArquiteturaBD));
  if (ArquiteturaSistema <> tarqSist_None) then
    AtributoAdd('arqSist',ArquiteturaSistemaToStr(ArquiteturaSistema));

  sArquivoBase64 := ArquivoBase64;

  FGerador.wCampo(tcStr, '', 'arquivo', 0, 0, 1, sArquivoBase64,'',True,sAtributo);

  if (Assinado) then
  begin
    FSignature.GerarXML;
    FGerador.ArquivoFormatoXML := FGerador.ArquivoFormatoXML + FSignature.Gerador.ArquivoFormatoXML;
  end;

  FGerador.wGrupo('/menuFiscal');

  FXMLOriginal := ConverteXMLtoUTF8(FGerador.ArquivoFormatoXML);

  if (Assinado) then
    FXMLAssinado := FXMLOriginal;
end;

procedure TACBrPAFNFCe_MenuFiscal.GerarXML(const Assinar: Boolean);
var
  Leitor: TLeitor;
begin
  GerarXMLMenuFiscal(False);
  if Assinar then
  begin
    FXMLAssinado := TACBrPAFNFCe(FACBrPAFNFCe).SSL.Assinar(FXMLOriginal, 'menuFiscal', 'arquivo');

    Leitor := TLeitor.Create;
    try
      leitor.Grupo := FXMLAssinado;
      FSignature.URI := Leitor.rAtributo('Reference URI=');
      FSignature.DigestValue := Leitor.rCampo(tcStr, 'DigestValue');
      FSignature.SignatureValue := Leitor.rCampo(tcStr, 'SignatureValue');
      FSignature.X509Certificate := Leitor.rCampo(tcStr, 'X509Certificate');
    finally
      Leitor.Free;
    end;

    GerarXMLMenuFiscal(True);
  end;
end;

procedure TACBrPAFNFCe_MenuFiscal.Inicializar;
begin
  FArquivo := '';
  FArquivoBase64 := '';
  FNumeroArquivo := tnroArq_None;
  FDataGeracaoArquivo := 0;
  FHoraGeracaoArquivo := 0;
  FArquiteturaBD := tarqBD_None;
  FArquiteturaSistema := tarqSist_None;
  FSignature := TSignature.Create;
end;

procedure TACBrPAFNFCe_MenuFiscal.LoadFromFile(const AFileName: String);
var
  lstArquivo: TStringList;
begin
  try
    lstArquivo := TStringList.Create;
    lstArquivo.LoadFromFile(AFileName);
    Arquivo := lstArquivo.Text;
    ArquivoBase64 := EncodeBase64(Arquivo);
  finally
    lstArquivo.Free;
  end;
end;

procedure TACBrPAFNFCe_MenuFiscal.SaveToFile(const AXmlFileName: String; const AAssinar: Boolean);
begin
  GerarXML(AAssinar);

  if FXMLAssinado <> '' then
    WriteToTXT(AXmlFileName, FXMLAssinado, False, True)
  else
    WriteToTXT(AXmlFileName, FXMLOriginal, False, True);
end;

end.
