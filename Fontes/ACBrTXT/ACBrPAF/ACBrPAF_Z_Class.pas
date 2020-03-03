{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Carlos H. Marian                                }
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

unit ACBrPAF_Z_Class;

interface

uses SysUtils, Classes, ACBrTXTClass,
     ACBrPAF_Z;

type

  { TPAF_Z }

  TPAF_Z = class(TACBrTXTClass)
  private
    FRegistroZ1: TRegistroZ1;       // FRegistroZ1
    FRegistroZ2: TRegistroZ2;       // FRegistroZ2
    FRegistroZ3: TRegistroZ3;       // FRegistroZ3
    FRegistroZ4: TRegistroZ4List;   // Lista de FRegistroZ4
    FRegistroZ9: TRegistroZ9;       // FRegistroZ9

    procedure CriaRegistros;
    procedure LiberaRegistros;
    function limpaCampo(pValor: String):String;
    procedure WriteRegistroZ2;
    procedure WriteRegistroZ3;
    procedure WriteRegistroZ4;
    procedure WriteRegistroZ9;
  public
    constructor Create;/// Create
    destructor Destroy; override; /// Destroy
    procedure LimpaRegistros;

    procedure WriteRegistroZ1;

    property RegistroZ1: TRegistroZ1 read FRegistroZ1 write FRegistroZ1;
    property RegistroZ2: TRegistroZ2 read FRegistroZ2 write FRegistroZ2;
    property RegistroZ3: TRegistroZ3 read FRegistroZ3 write FRegistroZ3;
    property RegistroZ4: TRegistroZ4List read FRegistroZ4 write FRegistroZ4;
    property RegistroZ9: TRegistroZ9 read FRegistroZ9 write FRegistroZ9;
  end;

implementation

uses ACBrTXTUtils, ACBrUtil, ACBrValidador;

{ TPAF_Z }

constructor TPAF_Z.Create;
begin
  inherited;
  CriaRegistros;
end;

procedure TPAF_Z.CriaRegistros;
begin
  FRegistroZ1 := TRegistroZ1.Create;
  FRegistroZ2 := TRegistroZ2.Create;
  FRegistroZ3 := TRegistroZ3.Create;
  FRegistroZ4 := TRegistroZ4List.Create;
  FRegistroZ9 := TRegistroZ9.Create;

  FRegistroZ9.TOT_REG := 0;
end;


destructor TPAF_Z.Destroy;
begin
  LiberaRegistros;
  inherited;
end;

procedure TPAF_Z.LiberaRegistros;
begin
  FRegistroZ1.Free;
  FRegistroZ2.Free;
  FRegistroZ3.Free;
  FRegistroZ4.Free;
  FRegistroZ9.Free;
end;

function TPAF_Z.limpaCampo(pValor: String): String;
begin
  pValor := StringReplace(pValor,'.', '', [rfReplaceAll] );
  pValor := StringReplace(pValor,'-', '', [rfReplaceAll] );
  pValor := StringReplace(pValor,'/', '', [rfReplaceAll] );
  pValor := StringReplace(pValor,',', '', [rfReplaceAll] );
  Result := pValor;
end;

procedure TPAF_Z.LimpaRegistros;
begin
  /// Limpa os Registros
  LiberaRegistros;
  /// Recriar os Registros Limpos
  CriaRegistros;
end;

procedure TPAF_Z.WriteRegistroZ1;
begin
  if Assigned(FRegistroZ1) then
  begin
    with FRegistroZ1 do
    begin
      Check(funChecaCNPJ(CNPJ), '(Z1) IDENTIFICAÇÃO DO USUÁRIO DO PAF-ECF: O CNPJ "%s" digitado é inválido!', [CNPJ]);
      Check(funChecaIE(IE, UF), '(Z1) IDENTIFICAÇÃO DO USUÁRIO DO PAF-ECF: A Inscrição Estadual "%s" digitada é inválida!', [IE]);
      ///
      Add(LFill('Z1') +
          LFill(limpaCampo(CNPJ)        , 14) +
          RFill(limpaCampo(IE)          , 14) +
          RFill(limpaCampo(IM)          , 14) +
          RFill(UpperCase(TiraAcentos(RAZAOSOCIAL)), 50));
    end;
    WriteRegistroZ2;
    WriteRegistroZ3;
    WriteRegistroZ4;
    WriteRegistroZ9;
  end;
end;

procedure TPAF_Z.WriteRegistroZ2;
begin
  if Assigned(FRegistroZ2) then
  begin
    with FRegistroZ2 do
    begin
      Check(funChecaCNPJ(CNPJ), '(Z2) IDENTIFICAÇÃO DA EMPRESA DESENVOLVEDORA DO PAF-ECF: O CNPJ "%s" digitado é inválido!', [CNPJ]);
      Check(funChecaIE(IE, UF), '(Z2) IDENTIFICAÇÃO DA EMPRESA DESENVOLVEDORA DO PAF-ECF: A Inscrição Estadual "%s" digitada é inválida!', [IE]);
      ///
      Add(LFill('Z2') +
          LFill(limpaCampo(CNPJ)        , 14) +
          RFill(limpaCampo(IE)          , 14) +
          RFill(limpaCampo(IM)          , 14) +
          RFill(Uppercase(TiraAcentos(RAZAOSOCIAL)), 50));
    end;
  end;
end;

procedure TPAF_Z.WriteRegistroZ3;
begin
  if Assigned(FRegistroZ3) then
  begin
    with FRegistroZ3 do
    begin
      Add(LFill('Z3') +
          RFill(UpperCase(LAUDO) , 10) +
          RFill(UpperCase(NOME)  , 50) +
          RFill(UpperCase(VERSAO), 10));
    end;
  end;
end;

procedure TPAF_Z.WriteRegistroZ4;
var
  intFor: integer;
  dataAtual: TDateTime;
begin
  if Assigned(FRegistroZ4) then
  begin
    dataAtual:= Now();

    for intFor := 0 to FRegistroZ4.Count - 1 do
    begin
      with FRegistroZ4.Items[intFor] do
      begin
        Check(ValidarCNPJouCPF(CPF_CNPJ) = '', '(Z4) Totalização de vendas a CPF/CNPJ: O CNPJ "%s" digitado é inválido!', [CPF_CNPJ]);

        Add( LFill('Z4') +
             LFill(limpaCampo(CPF_CNPJ)      , 14) +
             LFill(VL_TOTAL  , 14, 2) +
             LFill(DATA_INI  , 'yyyymmdd') +
             LFill(DATA_FIM  , 'yyyymmdd') +
             LFill(dataAtual , 'yyyymmdd') +
             LFill(dataAtual , 'hhmmss'));
      end;

      FRegistroZ9.TOT_REG := FRegistroZ9.TOT_REG + 1;
    end;
  end;
end;

procedure TPAF_Z.WriteRegistroZ9;
begin
  if Assigned(FRegistroZ9) then
  begin
    with FRegistroZ9 do
    begin
      Check(funChecaCNPJ(FRegistroZ2.CNPJ),             '(Z9) TOTALIZAÇÃO: O CNPJ "%s" digitado é inválido!', [FRegistroZ2.CNPJ]);
      Check(funChecaIE(FRegistroZ2.IE, FRegistroZ2.UF), '(Z9) TOTALIZAÇÃO: A Inscrição Estadual "%s" digitada é inválida!', [FRegistroZ2.IE]);
      ///
      Add(LFill('Z9') +
          LFill(limpaCampo(FRegistroZ2.CNPJ), 14  ) +
          RFill(limpaCampo(FRegistroZ2.IE)  , 14  ) +
          LFill(TOT_REG         , 6, 0));
    end;
  end;
end;

end.
