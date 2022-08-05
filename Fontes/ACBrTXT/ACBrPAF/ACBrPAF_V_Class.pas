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

{$I ACBr.inc}

unit ACBrPAF_V_Class;

interface

uses
  SysUtils,
  Classes,
  ACBrTXTClass,
  ACBrPAF_V,
  ACBrPAFRegistros;

type

  { TPAF_V }

  TPAF_V = class(TACBrTXTClass)
  private
    FRegistroV1: TRegistroV1;       // FRegistroV1
    FRegistroV2: TRegistroV2;       // FRegistroV2
    FRegistrosV2 : TRegistroV2List;       // FRegistroV2List
    FRegistroV3: TRegistroV3;       // FRegistroV3
    FRegistrosV3 : TRegistroV3List;       // FRegistroV3List
    FRegistroV4: TRegistroV4;
    FRegistrosV4: TRegistroV4List;   // Lista de FRegistroV4
    FRegistroV9: TRegistroV9;       // FRegistroV9

    procedure CriaRegistros;
    procedure LiberaRegistros;
    function limpaCampo(pValor: String):String;
    procedure WriteRegistroV2(Layout: TLayoutPAF);
    procedure WriteRegistroV3(Layout: TLayoutPAF);
    procedure WriteRegistroV4(Layout: TLayoutPAF);
    procedure WriteRegistroV9;
  public
    constructor Create;/// Create
    destructor Destroy; override; /// Destroy
    procedure LimpaRegistros;

    procedure WriteRegistroV1(Layout: TLayoutPAF);

    property RegistroV1: TRegistroV1 read FRegistroV1 write FRegistroV1;
    property RegistroV2: TRegistroV2 read FRegistroV2 write FRegistroV2;
    property RegistrosV2 : TRegistroV2List read FRegistrosV2 write FRegistrosV2;
    property RegistroV3: TRegistroV3 read FRegistroV3 write FRegistroV3;
    property RegistrosV3 : TRegistroV3List read FRegistrosV3 write FRegistrosV3;
    property RegistroV4: TRegistroV4 read FRegistroV4 write FRegistroV4;
    property RegistrosV4: TRegistroV4List read FRegistrosV4 write FRegistrosV4;
    property RegistroV9: TRegistroV9 read FRegistroV9 write FRegistroV9;
  end;

implementation

uses ACBrTXTUtils, ACBrUtil.Strings, ACBrValidador;

{ TPAF_V }

constructor TPAF_V.Create;
begin
  inherited;
  CriaRegistros;
end;

procedure TPAF_V.CriaRegistros;
begin
  FRegistroV1 := TRegistroV1.Create;
  FRegistroV2 := TRegistroV2.Create;
  FRegistrosV2 := TRegistroV2List.Create;
  FRegistroV3 := TRegistroV3.Create;
  FRegistrosV3 := TRegistroV3List.Create;
  FRegistroV4 := TRegistroV4.Create;
  FRegistrosV4 := TRegistroV4List.Create;
  FRegistroV9 := TRegistroV9.Create;

  FRegistroV9.TOT_REG := 0;
end;


destructor TPAF_V.Destroy;
begin
  LiberaRegistros;
  inherited;
end;

procedure TPAF_V.LiberaRegistros;
begin
  FRegistroV1.Free;
  FRegistroV2.Free;
  FRegistrosV2.Free;
  FRegistroV3.Free;
  FRegistrosV3.Free;
  FRegistroV4.Free;
  FRegistrosV4.Free;
  FRegistroV9.Free;
end;

function TPAF_V.limpaCampo(pValor: String): String;
begin
  pValor := StringReplace(pValor,'.', '', [rfReplaceAll] );
  pValor := StringReplace(pValor,'-', '', [rfReplaceAll] );
  pValor := StringReplace(pValor,'/', '', [rfReplaceAll] );
  pValor := StringReplace(pValor,',', '', [rfReplaceAll] );
  Result := pValor;
end;

procedure TPAF_V.LimpaRegistros;
begin
  /// Limpa os Registros
  LiberaRegistros;
  /// Recriar os Registros Limpos
  CriaRegistros;
end;

procedure TPAF_V.WriteRegistroV1(Layout: TLayoutPAF);
begin
  if Assigned(FRegistroV1) then
  begin
    with FRegistroV1 do
    begin
      Check(funChecaCNPJ(CNPJ), '(V1) IDENTIFICAÇÃO DO USUÁRIO DO PAF-ECF: O CNPJ "%s" digitado é inválido!', [CNPJ]);
      Check(funChecaIE(IE, UF), '(V1) IDENTIFICAÇÃO DO USUÁRIO DO PAF-ECF: A Inscrição Estadual "%s" digitada é inválida!', [IE]);
      ///
      Add(LFill('V1') +
          LFill(limpaCampo(CNPJ)        , 14) +
          RFill(limpaCampo(IE)          , 14) +
          RFill(limpaCampo(IM)          , 14) +
          RFill(TiraAcentos(RAZAOSOCIAL), 50));
    end;
    WriteRegistroV2(Layout);
    WriteRegistroV3(Layout);
    WriteRegistroV4(Layout);
    if Layout = lpPAFECF then
      WriteRegistroV9;
  end;
end;

procedure TPAF_V.WriteRegistroV2(Layout: TLayoutPAF);
var
  intFor:integer;
begin
  if Layout = lpPAFECF then
  begin
    if Assigned(FRegistroV2) then
    begin
      with FRegistroV2 do
      begin
        Check(funChecaCNPJ(CNPJ), '(V2) IDENTIFICAÇÃO DA EMPRESA DESENVOLVEDORA DO PAF-ECF: O CNPJ "%s" digitado é inválido!', [CNPJ]);
        Check(funChecaIE(IE, UF), '(V2) IDENTIFICAÇÃO DA EMPRESA DESENVOLVEDORA DO PAF-ECF: A Inscrição Estadual "%s" digitada é inválida!', [IE]);
        ///
        Add(LFill('V2') +
            LFill(limpaCampo(CNPJ)        , 14) +
            RFill(limpaCampo(IE)          , 14) +
            RFill(limpaCampo(IM)          , 14) +
            RFill(TiraAcentos(RAZAOSOCIAL), 50));
      end;
    end;
  end
  else
  begin
    if Assigned(FRegistrosV2)then
    begin
      for intFor := 0 to FRegistrosV2.Count - 1 do
      begin
        with FRegistrosV2.Items[intFor] do
        begin
          Add( LFill('V2') +
               LFill(DATA) +
               LFill(DAV,13));
        end;
      end;
    end;
  end;
end;

procedure TPAF_V.WriteRegistroV3(Layout: TLayoutPAF);
var
  intFor:integer;
begin
  if Layout = lpPAFECF then
  begin
    if Assigned(FRegistroV3) then
    begin
      with FRegistroV3 do
      begin
        Add(LFill('V3') +
            RFill(LAUDO , 10) +
            RFill(NOME  , 50) +
            RFill(VERSAO, 10));
      end;
    end;
  end
  else
  begin
    if Assigned(FRegistrosV3)then
    begin
      for intFor := 0 to FRegistrosV3.Count - 1 do
      begin
        with FRegistrosV3.Items[intFor] do
        begin
          Add( LFill('V2') +
               LFill(DAV,13));
        end;
      end;
    end;
  end;
end;

procedure TPAF_V.WriteRegistroV4(Layout: TLayoutPAF);
var
  intFor: integer;
begin
  if Layout = lpPAFECF then
  begin
    if Assigned(FRegistrosV4) then
    begin
      for intFor := 0 to FRegistrosV4.Count - 1 do
      begin
        with FRegistrosV4.Items[intFor] do
        begin
          Add( LFill('V4') +
               LFill(limpaCampo(NUMUMEROFABRICACAO), 20) +
               LFill(MFADICIONAL  , 1) +
               LFill(limpaCampo(MARCAECF) , 20) +
               LFill(limpaCampo(MODELOECF),20));
        end;
        FRegistroV9.TOT_REG := FRegistroV9.TOT_REG + 1;
      end;
    end;
  end
  else
  begin
    if Assigned(FRegistroV4) then
      Add( LFill('V4') +
        LFill(FRegistroV4.DATA));
  end;
end;

procedure TPAF_V.WriteRegistroV9;
begin
  if Assigned(FRegistroV9) then
  begin
    with FRegistroV9 do
    begin
      Check(funChecaCNPJ(FRegistroV2.CNPJ),             '(V9) TOTALIZAÇÃO: O CNPJ "%s" digitado é inválido!', [FRegistroV2.CNPJ]);
      Check(funChecaIE(FRegistroV2.IE, FRegistroV2.UF), '(V9) TOTALIZAÇÃO: A Inscrição Estadual "%s" digitada é inválida!', [FRegistroV2.IE]);
      ///
      Add(LFill('V9') +
          LFill(limpaCampo(FRegistroV2.CNPJ), 14  ) +
          RFill(limpaCampo(FRegistroV2.IE)  , 14  ) +
          LFill(TOT_REG         , 6, 0));
    end;
  end;
end;

end.

