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

{$I ACBr.inc}

unit ACBrPAF_W_Class;

interface

uses SysUtils, Classes, ACBrTXTClass,
     ACBrPAF_W;

type

  { TPAF_W }

  TPAF_W = class(TACBrTXTClass)
  private
    FRegistroW1: TRegistroW1;       // FRegistroW1
    FRegistroW2: TRegistroW2;       // FRegistroW2
    FRegistroW3: TRegistroW3;       // FRegistroW3
    FRegistroW4: TRegistroW4List;   // Lista de FRegistroW4
    FRegistroW9: TRegistroW9;       // FRegistroW9

    procedure CriaRegistros;
    procedure LiberaRegistros;
    function limpaCampo(pValor: String):String;
    procedure WriteRegistroW2;
    procedure WriteRegistroW3;
    procedure WriteRegistroW4;
    procedure WriteRegistroW9;
  public
    constructor Create;/// Create
    destructor Destroy; override; /// Destroy
    procedure LimpaRegistros;

    procedure WriteRegistroW1;

    property RegistroW1: TRegistroW1 read FRegistroW1 write FRegistroW1;
    property RegistroW2: TRegistroW2 read FRegistroW2 write FRegistroW2;
    property RegistroW3: TRegistroW3 read FRegistroW3 write FRegistroW3;
    property RegistroW4: TRegistroW4List read FRegistroW4 write FRegistroW4;
    property RegistroW9: TRegistroW9 read FRegistroW9 write FRegistroW9;
  end;

implementation

uses ACBrTXTUtils, ACBrUtil, ACBrValidador;

{ TPAF_W }

constructor TPAF_W.Create;
begin
  inherited;
  CriaRegistros;
end;

procedure TPAF_W.CriaRegistros;
begin
  FRegistroW1 := TRegistroW1.Create;
  FRegistroW2 := TRegistroW2.Create;
  FRegistroW3 := TRegistroW3.Create;
  FRegistroW4 := TRegistroW4List.Create;
  FRegistroW9 := TRegistroW9.Create;

  FRegistroW9.TOT_REG := 0;
end;


destructor TPAF_W.Destroy;
begin
  LiberaRegistros;
  inherited;
end;

procedure TPAF_W.LiberaRegistros;
begin
  FRegistroW1.Free;
  FRegistroW2.Free;
  FRegistroW3.Free;
  FRegistroW4.Free;
  FRegistroW9.Free;
end;

function TPAF_W.limpaCampo(pValor: String): String;
begin
  pValor := StringReplace(pValor,'.', '', [rfReplaceAll] );
  pValor := StringReplace(pValor,'-', '', [rfReplaceAll] );
  pValor := StringReplace(pValor,'/', '', [rfReplaceAll] );
  pValor := StringReplace(pValor,',', '', [rfReplaceAll] );
  Result := pValor;
end;

procedure TPAF_W.LimpaRegistros;
begin
  /// Limpa os Registros
  LiberaRegistros;
  /// Recriar os Registros Limpos
  CriaRegistros;
end;

procedure TPAF_W.WriteRegistroW1;
begin
  if Assigned(FRegistroW1) then
  begin
    with FRegistroW1 do
    begin
      Check(funChecaCNPJ(CNPJ), '(W1) IDENTIFICAÇÃO DO USUÁRIO DO PAF-ECF: O CNPJ "%s" digitado é inválido!', [CNPJ]);
      Check(funChecaIE(IE, UF), '(W1) IDENTIFICAÇÃO DO USUÁRIO DO PAF-ECF: A Inscrição Estadual "%s" digitada é inválida!', [IE]);
      ///
      Add(LFill('W1') +
          LFill(limpaCampo(CNPJ)        , 14) +
          RFill(limpaCampo(IE)          , 14) +
          RFill(limpaCampo(IM)          , 14) +
          RFill(TiraAcentos(RAZAOSOCIAL), 50));
    end;
    WriteRegistroW2;
    WriteRegistroW3;
    WriteRegistroW4;
    WriteRegistroW9;
  end;
end;

procedure TPAF_W.WriteRegistroW2;
begin
  if Assigned(FRegistroW2) then
  begin
    with FRegistroW2 do
    begin
      Check(funChecaCNPJ(CNPJ), '(W2) IDENTIFICAÇÃO DA EMPRESA DESENVOLVEDORA DO PAF-ECF: O CNPJ "%s" digitado é inválido!', [CNPJ]);
      Check(funChecaIE(IE, UF), '(W2) IDENTIFICAÇÃO DA EMPRESA DESENVOLVEDORA DO PAF-ECF: A Inscrição Estadual "%s" digitada é inválida!', [IE]);
      ///
      Add(LFill('W2') +
          LFill(limpaCampo(CNPJ)        , 14) +
          RFill(limpaCampo(IE)          , 14) +
          RFill(limpaCampo(IM)          , 14) +
          RFill(TiraAcentos(RAZAOSOCIAL), 50));
    end;
  end;
end;

procedure TPAF_W.WriteRegistroW3;
begin
  if Assigned(FRegistroW3) then
  begin
    with FRegistroW3 do
    begin
      Add(LFill('W3') +
          RFill(LAUDO , 10) +
          RFill(NOME  , 50) +
          RFill(VERSAO, 10));
    end;
  end;
end;

procedure TPAF_W.WriteRegistroW4;
var
  intFor: integer;
begin
  if Assigned(FRegistroW4) then
  begin
    for intFor := 0 to FRegistroW4.Count - 1 do
    begin
      with FRegistroW4.Items[intFor] do
      begin
        Add( LFill('W4') +
             LFill(ORIGEMDARE, 20) +
             LFill(STATUSDARE  , 1) +
             LFill(CRE, 9) +
             RFill(DAV, 13) +
             RFill(PREVENDA, 10) +
             LFill(CCF, 9) +
             LFill(VALORTOTALDARE, 14, 2)
             LFill(limpaCampo(NUMUMEROFABRICACAO), 20));
      end;
      FRegistroW9.TOT_REG := FRegistroW9.TOT_REG + 1;
    end;
  end;
end;

procedure TPAF_W.WriteRegistroW9;
begin
  if Assigned(FRegistroW9) then
  begin
    with FRegistroW9 do
    begin
      Check(funChecaCNPJ(FRegistroW2.CNPJ),             '(W9) TOTALIZAÇÃO: O CNPJ "%s" digitado é inválido!', [FRegistroW2.CNPJ]);
      Check(funChecaIE(FRegistroW2.IE, FRegistroW2.UF), '(W9) TOTALIZAÇÃO: A Inscrição Estadual "%s" digitada é inválida!', [FRegistroW2.IE]);
      ///
      Add(LFill('W9') +
          LFill(limpaCampo(FRegistroW2.CNPJ), 14  ) +
          RFill(limpaCampo(FRegistroW2.IE)  , 14  ) +
          LFill(TOT_REG         , 6, 0));
    end;
  end;
end;

end.

