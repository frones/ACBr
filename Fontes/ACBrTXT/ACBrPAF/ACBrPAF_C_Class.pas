{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Isaque Pinheiro                                 }
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

unit ACBrPAF_C_Class;

interface

uses SysUtils, Classes, DateUtils, ACBrTXTClass,
     ACBrPAF_C;

type

  { TPAF_C }

  TPAF_C = class(TACBrTXTClass)
  private
    FRegistroC1: TRegistroC1;       /// FRegistroC1
    FRegistroC2: TRegistroC2List;   /// Lista de FRegistroC2
    FRegistroC9: TRegistroC9;       /// FRegistroC9

    procedure CriaRegistros;
    procedure LiberaRegistros;
  public
    constructor Create; /// Create
    destructor Destroy; override; /// Destroy
    procedure LimpaRegistros;

    procedure WriteRegistroC1;
    procedure WriteRegistroC2;
    procedure WriteRegistroC9;

    property RegistroC1: TRegistroC1 read FRegistroC1 write FRegistroC1;
    property RegistroC2: TRegistroC2List read FRegistroC2 write FRegistroC2;
    property RegistroC9: TRegistroC9 read FRegistroC9 write FRegistroC9;
  end;

implementation

uses strutils,
     ACBrTXTUtils;

{ TPAF_C }

constructor TPAF_C.Create;
begin
  inherited;
  CriaRegistros;
end;

procedure TPAF_C.CriaRegistros;
begin
  FRegistroC1 := TRegistroC1.Create;
  FRegistroC2 := TRegistroC2List.Create;
  FRegistroC9 := TRegistroC9.Create;

  FRegistroC9.TOT_REG := 0;
end;

destructor TPAF_C.Destroy;
begin
  LiberaRegistros;
  inherited;
end;

procedure TPAF_C.LiberaRegistros;
begin
  FRegistroC1.Free;
  FRegistroC2.Free;
  FRegistroC9.Free;
end;

procedure TPAF_C.LimpaRegistros;
begin
  /// Limpa os Registros
  LiberaRegistros;
  /// Recriar os Registros Limpos
  CriaRegistros;
end;

procedure TPAF_C.WriteRegistroC1;
begin
   if Assigned(FRegistroC1) then
   begin
      with FRegistroC1 do
      begin
        Check(funChecaCNPJ(CNPJ), '(C1) ESTABELECIMENTO: O CNPJ "%s" digitado é inválido!', [CNPJ]);
        Check(funChecaIE(IE, UF), '(C1) ESTABELECIMENTO: A Inscrição Estadual "%s" digitada é inválida!', [IE]);
        ///
        Add(LFill('C1') +
            LFill(CNPJ, 14) +
            RFill(IE, 14) +
            RFill(IM, 14) +
            RFill(RAZAOSOCIAL, 50, ifThen(not InclusaoExclusao, ' ', '?')));
      end;
   end;
end;

function OrdenarC2(ACampo1, ACampo2: Pointer): Integer;
var
  Campo1, Campo2: String;
begin
  Campo1 := TRegistroC2(ACampo1).BOMBA +
            TRegistroC2(ACampo1).BICO +
            Format('%.15d', [Trunc(TRegistroC2(ACampo1).ENCERRANTE_INICIAL * 100)]);
  Campo2 := TRegistroC2(ACampo2).BOMBA +
            TRegistroC2(ACampo2).BICO +
            Format('%.15d', [Trunc(TRegistroC2(ACampo2).ENCERRANTE_INICIAL * 100)]);

  Result := AnsiCompareText(Campo1, Campo2);
end;

procedure TPAF_C.WriteRegistroC2;
var
intFor: integer;
begin
  if Assigned(FRegistroC2) then
  begin
     FRegistroC2.Sort(@OrdenarC2);

     Check(funChecaCNPJ(FRegistroC1.CNPJ), '(C2) CONTROLE DE ABASTECIMENTO E ENCERRANTE: O CNPJ "%s" digitado é inválido!', [FRegistroC1.CNPJ]);
     for intFor := 0 to FRegistroC2.Count - 1 do
     begin
        with FRegistroC2.Items[intFor] do
        begin
          ///
          Add( LFill('C2') +
               LFill(FRegistroC1.CNPJ, 14) +
               LFill(ID_ABASTECIMENTO, 15) +
               RFill(TANQUE, 3) +
               RFill(BOMBA, 3) +
               RFill(BICO, 3) +
               RFill(COMBUSTIVEL, 20, ifThen(RegistroValido, ' ', '?')) +
               LFill(DATA_ABASTECIMENTO, 'yyyymmdd') +
               LFill(HORA_ABASTECIMENTO, 'hhmmss') +
               LFill(ENCERRANTE_INICIAL, 15, 2) +
               LFill(ENCERRANTE_FINAL, 15, 2) +
               RFill(STATUS_ABASTECIMENTO, 10) +
               IfThen(Pos(STATUS_ABASTECIMENTO,'|EMITIDOCFN|EMITIDOCFM|EMITIDOCFA|EMITIDOCFC|') > 0, RFill(NRO_SERIE_ECF, 20), RFill('',20)) +
               IfThen(Pos(STATUS_ABASTECIMENTO,'|EMITIDOCFN|EMITIDOCFM|EMITIDOCFA|EMITIDOCFC|') > 0, LFill(DATA, 'yyyymmdd'), LFill(0,8)) +
               IfThen(Pos(STATUS_ABASTECIMENTO,'|EMITIDOCFN|EMITIDOCFM|EMITIDOCFA|EMITIDOCFC|') > 0, LFill(HORA, 'hhmmss'), LFill(0,6)) +
               LFill(COO, 9) +
               LFill(NRO_NOTA_FISCAL, 6) +
               LFill(VOLUME, 10, 3) );
        end;
        ///
        FRegistroC9.TOT_REG := FRegistroC9.TOT_REG + 1;
     end;
  end;
end;

procedure TPAF_C.WriteRegistroC9;
begin
   if Assigned(FRegistroC9) then
   begin
      with FRegistroC9 do
      begin
        Check(funChecaCNPJ(FRegistroC1.CNPJ),             '(C9) TOTALIZAÇÃO: O CNPJ "%s" digitado é inválido!', [FRegistroC1.CNPJ]);
        Check(funChecaIE(FRegistroC1.IE, FRegistroC1.UF), '(C9) TOTALIZAÇÃO: A Inscrição Estadual "%s" digitada é inválida!', [FRegistroC1.IE]);
        ///
        Add(LFill('C9') +
            LFill(FRegistroC1.CNPJ, 14) +
            RFill(FRegistroC1.IE, 14) +
            LFill(TOT_REG, 6, 0));
      end;
   end;
end;

end.

