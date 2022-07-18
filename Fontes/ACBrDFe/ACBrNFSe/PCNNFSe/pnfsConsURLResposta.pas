{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Jurisato Junior                           }
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

unit pnfsConsURLResposta;

interface

uses
  SysUtils, Classes,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$IFEND}
  ACBrBase, ACBrUtil.Strings,
  pcnAuxiliar, pcnConversao, pcnLeitor, pnfsConversao, pnfsNFSe;

type
  TInfURL = class(TObject)
  private
    FInformacoesLote: TInformacoesLote;
    FURL: String;

  public
    constructor Create;
    destructor Destroy; override;

    property URL: String read FURL write FURL;
    property InformacoesLote: TInformacoesLote read FInformacoesLote write FInformacoesLote;
  end;

 { TretURL }

 TretURL = class(TObject)
  private
    FLeitor: TLeitor;
    FInfURL: TInfURL;
    FProvedor: TnfseProvedor;
  public
    constructor Create;
    destructor Destroy; override;

    function LerXml: Boolean;

    function LerXml_ABRASF: Boolean;
//
//    function LerXml_proCONAM: Boolean;
//    function LerXML_proEL: Boolean;
//    function LerXML_proEquiplano: Boolean;
//    function LerXML_proInfisc: Boolean;
//    function LerXml_proISSDSF: Boolean;
//    function LerXml_proNFSeBrasil: Boolean;
//    function LerXml_proSP: Boolean;
//    function LerXML_proAssessorPublico: boolean;
//    function LerXML_proSiat: Boolean;
    property Leitor: TLeitor         read FLeitor   write FLeitor;
    property InfURL: TInfURL         read FInfURL   write FInfURL;
    property Provedor: TnfseProvedor read FProvedor write FProvedor;
  end;


implementation

{ TInfURL }

constructor TInfURL.Create;
begin
  inherited Create;
  FInformacoesLote := TInformacoesLote.Create;
  URL := '';
end;

destructor TInfURL.Destroy;
begin
  FInformacoesLote.Free;

  inherited;
end;

{ TretURL }

constructor TretURL.Create;
begin
  inherited Create;

  FLeitor := TLeitor.Create;
  FInfURL := TInfURL.Create;
end;

destructor TretURL.Destroy;
begin
  FLeitor.Free;
  FInfURL.Free;

  inherited;
end;

function TretURL.LerXml: Boolean;
begin
  if Provedor = proISSCuritiba then
    Leitor.Arquivo := RemoverNameSpace(Leitor.Arquivo)
  else
    Leitor.Arquivo := RemoverNameSpace(RemoverAtributos(RetirarPrefixos(Leitor.Arquivo, Provedor), Provedor));

  Leitor.Grupo := Leitor.Arquivo;

//  case Provedor of
//    proCONAM:      Result := LerXml_proCONAM;
//    proISSDSF:     Result := LerXml_proISSDSF;
//    proEquiplano:  Result := LerXML_proEquiplano;
//    proInfisc,
//    proInfiscv11:  Result := LerXml_proInfisc;
//    proEL:         Result := LerXML_proEL;
//    proNFSeBrasil: Result := LerXml_proNFSeBrasil;
//    proSP,
//    proNotaBlu:    Result := LerXml_proSP;
//    proAssessorPublico: Result := LerXML_proAssessorPublico;
//    proSiat:       Result := LerXML_proSiat;
//  else
    Result := LerXml_ABRASF;
//  end;
end;

function TretURL.LerXml_ABRASF: Boolean;
begin
  Result := True;

  try
    if (leitor.rExtrai(1, 'ConsultarUrlVisualizacaoNfseResposta ') <> '') or
       (leitor.rExtrai(1, 'ConsultarUrlVisualizacaoNfseResult') <> '') then
    begin
      InfURL.FURL := Leitor.rCampo(tcStr, 'UrlVisualizacao');
    end;

  except
    Result := False;
  end;
end;

end.
