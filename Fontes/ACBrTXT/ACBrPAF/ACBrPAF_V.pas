{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Juliomar Marchetti                              }
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
unit ACBrPAF_V;

interface

uses
  SysUtils, Classes, Contnrs, DateUtils, ACBrPAFRegistros;

type
  /// REGISTRO TIPO V1 - IDENTIFICAÇÃO DO USUÁRIO DO PAF-ECF:
  TRegistroV1 = class(TRegistroX1);

  // REGISTRO TIPO V2 - IDENTIFICAÇÃO DA EMPRESA DESENVOLVEDORA DO PAF-ECF:
  TRegistroV2 = Class(TRegistroX1)
  private
    fDATA : TDateTime;
    fDAV : integer;
  public
    property DATA : TDateTime read fDATA write fDATA;
    property DAV : Integer read fDAV write fDAV;
  end;

  /// REGISTRO TIPO V2 – Lista

  { TRegistroV2List }

  TRegistroV2List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroV2;
    procedure SetItem(Index: Integer; const Value: TRegistroV2);
  public
    function New: TRegistroV2;
    property Items[Index: Integer]: TRegistroV2 read GetItem write SetItem;
  end;

  // REGISTRO TIPO V3 - IDENTIFICAÇÃO DO PAF-ECF
  TRegistroV3 = Class(TRegistroX3)
  private
    fDATA : TDateTime;
    fDAV : integer;
  public
    property DATA : TDateTime read fDATA write fDATA;
    property DAV : Integer read fDAV write fDAV;
  end;

  /// REGISTRO TIPO V3 – Lista

  { TRegistroV3List }

  TRegistroV3List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroV3;
    procedure SetItem(Index: Integer; const Value: TRegistroV3);
  public
    function New: TRegistroV3;
    property Items[Index: Integer]: TRegistroV3 read GetItem write SetItem;
  end;

  //REGISTRO TIPO V4 - Relação dos equipamentos ECF autorizados a funcionar com o PAF-ECF:

  { TRegistroV4 }

  TRegistroV4 = Class
  private
    FDATA: TDateTime;
    fMARCAECF: string;
    fMFADICIONAL: string;
    fMODELOECF: string;
    fNUMEROFABRICACAO: String;
  public
    property NUMUMEROFABRICACAO : String read fNUMEROFABRICACAO write fNUMEROFABRICACAO;
    property MFADICIONAL : string read fMFADICIONAL write fMFADICIONAL;
    property MARCAECF : string read fMARCAECF write fMARCAECF;
    property MODELOECF : string read fMODELOECF write fMODELOECF;

    property DATA: TDateTime read FDATA write FDATA;
  end;

  /// REGISTRO TIPO V4 – Lista
  TRegistroV4List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroV4;
    procedure SetItem(Index: Integer; const Value: TRegistroV4);
  public
    function New: TRegistroV4;
    property Items[Index: Integer]: TRegistroV4 read GetItem write SetItem;
  end;

  // REGISTRO TIPO V9 - TOTALIZAÇÃO DO ARQUIVO
  TRegistroV9 = Class(TRegistroX9);

  // REGISTRO TIPO EAD - ASSINATURA DIGITAL

implementation

{ TRegistroV3List }

function TRegistroV3List.GetItem(Index: Integer): TRegistroV3;
begin
   Result := TRegistroV3(inherited Items[Index]);
end;

procedure TRegistroV3List.SetItem(Index: Integer; const Value: TRegistroV3);
begin
  Put(Index, Value);
end;

function TRegistroV3List.New: TRegistroV3;
begin
  Result := TRegistroV3.Create;
  Add(Result);
end;

{ TRegistroV2List }

function TRegistroV2List.GetItem(Index: Integer): TRegistroV2;
begin
  Result := TRegistroV2(inherited Items[Index]);
end;

procedure TRegistroV2List.SetItem(Index: Integer; const Value: TRegistroV2);
begin
  Put(Index, Value);
end;

function TRegistroV2List.New: TRegistroV2;
begin
  Result := TRegistroV2.Create;
  Add(Result);
end;

{ TRegistroV4List }

function TRegistroV4List.GetItem(Index: Integer): TRegistroV4;
begin
  Result := TRegistroV4(inherited Items[Index]);
end;

procedure TRegistroV4List.SetItem(Index: Integer; const Value: TRegistroV4);
begin
  Put(Index, Value);
end;

function TRegistroV4List.New: TRegistroV4;
begin
  Result := TRegistroV4.Create;
  Add(Result);
end;

end.

