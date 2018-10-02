{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2016   Juliomar Marchetti                   }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* 05/09/2016: Juliomar Marchetti
|*  - Criação e distribuição da Primeira Versao
*******************************************************************************}
unit ACBrPAF_W;

interface

uses
  SysUtils, Classes, Contnrs, DateUtils, ACBrPAFRegistros;

type
  /// REGISTRO TIPO W1 - IDENTIFICAÇÃO DO USUÁRIO DO PAF-ECF:
  TRegistroW1 = class(TRegistroX1);

  // REGISTRO TIPO W2 - IDENTIFICAÇÃO DA EMPRESA DESENVOLVEDORA DO PAF-ECF:
  TRegistroW2 = Class(TRegistroX1);

  // REGISTRO TIPO W3 - IDENTIFICAÇÃO DO PAF-ECF
  TRegistroW3 = Class(TRegistroX3);

  //REGISTRO TIPO W4 - Identificação da RE:
  TRegistroW4 = Class
  private
    fORIGEMDARE: String;
    fSTATUSDARE: String;
    fCRE: Integer;
    fDAV: String;
    fPREVENDA: String;
    fCCF: Integer;
    fVALORTOTALDARE: Currency;
    fNUMEROFABRICACAO: String;
  public
    property ORIGEMDARE : String read fORIGEMDARE write fORIGEMDARE;
    property STATUSDARE : String read fSTATUSDARE write fSTATUSDARE;
    property CRE : Integer read fCRE write fCRE;
    property DAV : String read fDAV write fDAV;
    property PREVENDA : String read fPREVENDA write fPREVENDA;
    property CCF : Integer read fCCF write fCCF;
    property VALORTOTALDARE : Currency read fVALORTOTALDARE write fVALORTOTALDARE;
    property NUMEROFABRICACAO : String read fNUMEROFABRICACAO write fNUMEROFABRICACAO;
  end;

  /// REGISTRO TIPO W4 – Lista
  TRegistroW4List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroW4;
    procedure SetItem(Index: Integer; const Value: TRegistroW4);
  public
    function New: TRegistroW4;
    property Items[Index: Integer]: TRegistroW4 read GetItem write SetItem;
  end;

  // REGISTRO TIPO W9 - TOTALIZAÇÃO DO ARQUIVO
  TRegistroW9 = Class(TRegistroX9);

  // REGISTRO TIPO EAD - ASSINATURA DIGITAL

implementation

{ TRegistroW4List }

function TRegistroW4List.GetItem(Index: Integer): TRegistroW4;
begin
  Result := TRegistroW4(inherited Items[Index]);
end;

procedure TRegistroW4List.SetItem(Index: Integer; const Value: TRegistroW4);
begin
  Put(Index, Value);
end;

function TRegistroW4List.New: TRegistroW4;
begin
  Result := TRegistroW4.Create;
  Add(Result);
end;

end.

