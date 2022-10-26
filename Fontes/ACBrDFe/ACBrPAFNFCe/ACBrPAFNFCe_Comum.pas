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

unit ACBrPAFNFCe_Comum;

interface

uses
  Classes, SysUtils, pcnGerador, ACBrBase;

type
  EACBrPAFNFCeException = class(Exception);

  TACBrPAFNFCe_NumeroArquivo = (tnroArq_None, tnroArq_Identificacao, tnroArq_Registros,
                                tnroArq_SaidasIdentCPFCNPJ, tnroArq_ReqExternasReg, tnroArq_MesasAbertas,
                                tnroArq_ControleDAV, tnroArq_ControleEncerrantes, tnroArq_AbastecimentosPendentes);
  TACBrPAFNFCe_ArquiteturaBD = (tarqBD_None, tarqBD_Local, tarqBD_Interno, tarqBD_Corporativo, tarqBD_Nuvem);
  TACBrPAFNFCe_ArquiteturaSistema = (tarqSist_None, tarqSist_Local, tarqSist_Interno, tarqSist_Corporativo, tarqSist_Nuvem);

  { TACBrPAFNFCe_BaseFile }

  TACBrPAFNFCe_BaseFile = class(TComponent)
  protected
    FACBrPAFNFCe: TComponent;
    FGerador: TGerador;
    FXMLOriginal: String;
    FXMLAssinado: String;
    FRemoverEncodingXMLAssinado: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property XMLOriginal: String read FXMLOriginal;
    property XMLAssinado: String read FXMLAssinado;
    property RemoverEncodingXMLAssinado: Boolean read FRemoverEncodingXMLAssinado write FRemoverEncodingXMLAssinado;

    procedure GerarXML(const Assinar: Boolean = True); virtual;
    procedure LoadFromFile(const AFileName: String); virtual;
    procedure SaveToFile(const AXmlFileName: String; const AAssinar: Boolean = True); virtual;
  end;

  function NumeroArquivoToStr(const AValue: TACBrPAFNFCe_NumeroArquivo): String;
  function ArquiteturaBDToStr(const AValue: TACBrPAFNFCe_ArquiteturaBD): String;
  function ArquiteturaSistemaToStr(const AValue: TACBrPAFNFCe_ArquiteturaSistema): String;

  function StrToNumeroArquivo(var OK: Boolean; const AValue: String): TACBrPAFNFCe_NumeroArquivo;
  function StrToArquiteturaBD(var OK: Boolean; const AValue: String): TACBrPAFNFCe_ArquiteturaBD;
  function StrToArquiteturaSistema(var OK: Boolean; const AValue: String): TACBrPAFNFCe_ArquiteturaSistema;

  function ZipFile(const DadosXML: String; const NomeArquivo: String): AnsiString;
  function UnZipFile(const DadosXML: String): AnsiString;

implementation

uses
  ACBrPAFNFCe, ACBrUtil.Strings, ACBrCompress, pcnConversao, synacode;

function NumeroArquivoToStr(const AValue: TACBrPAFNFCe_NumeroArquivo): String;
begin
  Result := EnumeradoToStr(AValue,
    ['', 'I', 'II', 'III', 'IV', 'V', 'VI', 'VII', 'VIII'],
    [tnroArq_None, tnroArq_Identificacao, tnroArq_Registros,
     tnroArq_SaidasIdentCPFCNPJ, tnroArq_ReqExternasReg, tnroArq_MesasAbertas,
     tnroArq_ControleDAV, tnroArq_ControleEncerrantes, tnroArq_AbastecimentosPendentes]
  );
end;

function ArquiteturaBDToStr(const AValue: TACBrPAFNFCe_ArquiteturaBD): String;
begin
  Result := EnumeradoToStr(AValue,
    ['', 'Banco de dados local', 'Banco de dados interno', 'Banco de dados corporativo', 'Banco de dados na nuvem'],
    [tarqBD_None, tarqBD_Local, tarqBD_Interno, tarqBD_Corporativo, tarqBD_Nuvem]
  );
end;

function ArquiteturaSistemaToStr(const AValue: TACBrPAFNFCe_ArquiteturaSistema): String;
begin
  Result := EnumeradoToStr(AValue,
    ['', 'PAF-NFC-e local', 'PAF-NFC-e interno', 'PAF-NFC-e corporativo', 'PAF-NFC-e nuvem'],
    [tarqSist_None, tarqSist_Local, tarqSist_Interno, tarqSist_Corporativo, tarqSist_Nuvem]
  );
end;

function StrToNumeroArquivo(var OK: Boolean; const AValue: String): TACBrPAFNFCe_NumeroArquivo;
begin
  Result := StrToEnumerado(OK, AValue,
    ['', 'I', 'II', 'III', 'IV', 'V', 'VI', 'VII', 'VIII'],
    [tnroArq_None, tnroArq_Identificacao, tnroArq_Registros,
     tnroArq_SaidasIdentCPFCNPJ, tnroArq_ReqExternasReg, tnroArq_MesasAbertas,
     tnroArq_ControleDAV, tnroArq_ControleEncerrantes, tnroArq_AbastecimentosPendentes]
  );
end;

function StrToArquiteturaBD(var OK: Boolean; const AValue: String): TACBrPAFNFCe_ArquiteturaBD;
begin
  Result := StrToEnumerado(OK, AValue,
    ['', 'Banco de dados local', 'Banco de dados interno', 'Banco de dados corporativo', 'Banco de dados na nuvem'],
    [tarqBD_None, tarqBD_Local, tarqBD_Interno, tarqBD_Corporativo, tarqBD_Nuvem]
  );
end;

function StrToArquiteturaSistema(var OK: Boolean; const AValue: String): TACBrPAFNFCe_ArquiteturaSistema;
begin
  Result := StrToEnumerado(OK, AValue,
    ['', 'PAF-NFC-e local', 'PAF-NFC-e interno', 'PAF-NFC-e corporativo', 'PAF-NFC-e nuvem'],
    [tarqSist_None, tarqSist_Local, tarqSist_Interno, tarqSist_Corporativo, tarqSist_Nuvem]
  );
end;

function ZipFile(const DadosXML: String; const NomeArquivo: String): AnsiString;
begin
  Result := ACBrCompress.ZipFileCompress(DadosXML, NomeArquivo);
end;

function UnZipFile(const DadosXML: String): AnsiString;
begin
  Result := ACBrCompress.ZipFileDeCompress(DadosXML);
end;

{ TACBrPAFNFCe_BaseFile }

constructor TACBrPAFNFCe_BaseFile.Create(AOwner: TComponent);
begin
  inherited;

  FACBrPAFNFCe := TACBrPAFNFCe(AOwner);
  FGerador := TGerador.Create;
  FRemoverEncodingXMLAssinado := False;
end;

destructor TACBrPAFNFCe_BaseFile.Destroy;
begin
  FGerador.Free;
  inherited;
end;

procedure TACBrPAFNFCe_BaseFile.GerarXML(const Assinar: Boolean = True);
begin
  raise EACBrPAFNFCeException.Create('Método não implementado "GerarXML"');
end;

procedure TACBrPAFNFCe_BaseFile.LoadFromFile(const AFileName: String);
begin
  raise EACBrPAFNFCeException.Create('Método não implementado "LoadFromFile"');
end;

procedure TACBrPAFNFCe_BaseFile.SaveToFile(const AXmlFileName: String; const AAssinar: Boolean);
begin
  raise EACBrPAFNFCeException.Create('Método não implementado "SaveToFile"');
end;

end.
