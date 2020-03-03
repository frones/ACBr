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
{******************************************************************************}

{$I ACBr.inc}

unit ACBrPAF_B_Class;

interface

uses SysUtils, Classes, DateUtils, ACBrTXTClass,
     ACBrPAF_B;

type

  { TPAF_B }

  TPAF_B = class(TACBrTXTClass)
  private
    FRegistroB1: TRegistroB1;       /// FRegistroB1
    FRegistroB2: TRegistroB2List;   /// Lista de FRegistroB2
    FRegistroB9: TRegistroB9;       /// FRegistroB9

    procedure CriaRegistros;
    procedure LiberaRegistros;
  public
    constructor Create; /// Create
    destructor Destroy; override; /// Destroy
    procedure LimpaRegistros;

    procedure WriteRegistroB1;
    procedure WriteRegistroB2;
    procedure WriteRegistroB9;

    property RegistroB1: TRegistroB1 read FRegistroB1 write FRegistroB1;
    property RegistroB2: TRegistroB2List read FRegistroB2 write FRegistroB2;
    property RegistroB9: TRegistroB9 read FRegistroB9 write FRegistroB9;
  end;

implementation

uses ACBrTXTUtils;

{ TPAF_B }

constructor TPAF_B.Create;
begin
  inherited;
  CriaRegistros;
end;

procedure TPAF_B.CriaRegistros;
begin
  FRegistroB1 := TRegistroB1.Create;
  FRegistroB2 := TRegistroB2List.Create;
  FRegistroB9 := TRegistroB9.Create;

  FRegistroB9.TOT_REG := 0;
end;

destructor TPAF_B.Destroy;
begin
  LiberaRegistros;
  inherited;
end;

procedure TPAF_B.LiberaRegistros;
begin
  FRegistroB1.Free;
  FRegistroB2.Free;
  FRegistroB9.Free;
end;

procedure TPAF_B.LimpaRegistros;
begin
  /// Limpa os Registros
  LiberaRegistros;
  /// Recriar os Registros Limpos
  CriaRegistros;
end;

procedure TPAF_B.WriteRegistroB1;
begin
   if Assigned(FRegistroB1) then
   begin
      with FRegistroB1 do
      begin
        Check(funChecaCNPJ(CNPJ), '(B1) ESTABELECIMENTO: O CNPJ "%s" digitado é inválido!', [CNPJ]);
        Check(funChecaIE(IE, UF), '(B1) ESTABELECIMENTO: A Inscrição Estadual "%s" digitada é inválida!', [IE]);
        ///
        Add(LFill('B1') +
            LFill(CNPJ, 14) +
            RFill(IE, 14) +
            RFill(IM, 14) +
            RFill(RAZAOSOCIAL, 50, ifThen(not InclusaoExclusao, ' ', '?')));
      end;
   end;
end;

function OrdenarB2(ACampo1, ACampo2: Pointer): Integer;
var
  Campo1, Campo2: String;
begin
  Campo1 := FormatDateTime('YYYYMMDD', TRegistroB2(ACampo1).DATA) +
            TRegistroB2(ACampo1).BOMBA +
            TRegistroB2(ACampo1).BICO;
  Campo2 := FormatDateTime('YYYYMMDD', TRegistroB2(ACampo1).DATA) +
            TRegistroB2(ACampo2).BOMBA +
            TRegistroB2(ACampo2).BICO;

  Result := AnsiCompareText(Campo1, Campo2);
end;

procedure TPAF_B.WriteRegistroB2;
var
intFor: integer;
begin
  if Assigned(FRegistroB2) then
  begin
     FRegistroB2.Sort(@OrdenarB2);

     Check(funChecaCNPJ(FRegistroB1.CNPJ), '(B2) REGISTRO DE SUBSTITUIÇÃO DA PLACA ELETRÓNICA DE GERENCIAMENTO DE BOMBA DE COMBUSTIVEL: O CNPJ "%s" digitado é inválido!', [FRegistroB1.CNPJ]);
     for intFor := 0 to FRegistroB2.Count - 1 do
     begin
        with FRegistroB2.Items[intFor] do
        begin
          ///
          Add( LFill('B2') +
               LFill(FRegistroB1.CNPJ, 14) +
               RFill(BOMBA, 3) +
               RFill(BICO, 3) +
               LFill(DATA, 'yyyymmdd') +
               LFill(HORA, 'hhmmss') +
               RFill(MOTIVO, 50, ifThen(RegistroValido, ' ', '?')) +
               LFill(CNPJ_EMPRESA, 14) +
               LFill(CPF_TECNICO, 11) +
               RFill(NRO_LACRE_ANTES, 15) +
               RFill(NRO_LACRE_APOS, 15) +
               LFill(ENCERRANTE_ANTES, 15, 2) +
               LFill(ENCERRANTE_APOS, 15, 2) );
        end;
        ///
        FRegistroB9.TOT_REG := FRegistroB9.TOT_REG + 1;
     end;
  end;
end;

procedure TPAF_B.WriteRegistroB9;
begin
   if Assigned(FRegistroB9) then
   begin
      with FRegistroB9 do
      begin
        Check(funChecaCNPJ(FRegistroB1.CNPJ),             '(B9) TOTALIZAÇÃO: O CNPJ "%s" digitado é inválido!', [FRegistroB1.CNPJ]);
        Check(funChecaIE(FRegistroB1.IE, FRegistroB1.UF), '(B9) TOTALIZAÇÃO: A Inscrição Estadual "%s" digitada é inválida!', [FRegistroB1.IE]);
        ///
        Add( LFill('B9') +
             LFill(FRegistroB1.CNPJ, 14) +
             RFill(FRegistroB1.IE, 14) +
             LFill(TOT_REG, 6, 0) );
      end;
   end;
end;

end.
