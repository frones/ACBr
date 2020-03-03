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

unit ACBrPAF_P_Class;

interface

uses SysUtils, Classes, DateUtils, ACBrTXTClass,
     ACBrPAF_P;

type
  /// TPAF_P -

  { TPAF_P }

  TPAF_P = class(TACBrTXTClass)
  private
    FRegistroP1: TRegistroP1;       /// FRegistroP1
    FRegistroP2: TRegistroP2List;   /// Lista de FRegistroP2
    FRegistroP9: TRegistroP9;       /// FRegistroP9

    procedure CriaRegistros;
    procedure LiberaRegistros;
  public
    constructor Create;/// Create
    destructor Destroy; override; /// Destroy
    procedure LimpaRegistros;

    procedure WriteRegistroP1;
    procedure WriteRegistroP2;
    procedure WriteRegistroP9;

    property RegistroP1: TRegistroP1 read FRegistroP1 write FRegistroP1;
    property RegistroP2: TRegistroP2List read FRegistroP2 write FRegistroP2;
    property RegistroP9: TRegistroP9 read FRegistroP9 write FRegistroP9;
  end;

implementation

uses ACBrTXTUtils;

{ TPAF_P }

constructor TPAF_P.Create;
begin
  inherited;
  CriaRegistros;
end;

procedure TPAF_P.CriaRegistros;
begin
  FRegistroP1 := TRegistroP1.Create;
  FRegistroP2 := TRegistroP2List.Create;
  FRegistroP9 := TRegistroP9.Create;

  FRegistroP9.TOT_REG := 0;
end;

destructor TPAF_P.Destroy;
begin
  LiberaRegistros;
  inherited;
end;

procedure TPAF_P.LiberaRegistros;
begin
  FRegistroP1.Free;
  FRegistroP2.Free;
  FRegistroP9.Free;
end;

procedure TPAF_P.LimpaRegistros;
begin
  /// Limpa os Registros
  LiberaRegistros;
  /// Recriar os Registros Limpos
  CriaRegistros;
end;

procedure TPAF_P.WriteRegistroP1;
begin
   if Assigned(FRegistroP1) then
   begin
      with FRegistroP1 do
      begin
        Check(funChecaCNPJ(CNPJ), '(P1) ESTABELECIMENTO: O CNPJ "%s" digitado é inválido!', [CNPJ]);
        Check(funChecaIE(IE, UF), '(P1) ESTABELECIMENTO: A Inscrição Estadual "%s" digitada é inválida!', [IE]);
        ///
        Add( LFill('P1') +
             LFill(CNPJ, 14) +
             RFill(IE, 14) +
             RFill(IM, 14) +
             RFill(RAZAOSOCIAL, 50 ,ifThen(not InclusaoExclusao, ' ', '?')) );
      end;
   end;
end;

function OrdenarP2(AProd1, AProd2: Pointer): Integer;
begin
  Result := AnsiCompareText(
    TRegistroP2(AProd1).COD_MERC_SERV,
    TRegistroP2(AProd2).COD_MERC_SERV
  );
end;

procedure TPAF_P.WriteRegistroP2;
var
intFor: integer;
begin
  if Assigned(FRegistroP2) then
  begin
     FRegistroP2.Sort(@OrdenarP2);

     Check(funChecaCNPJ(FRegistroP1.CNPJ), '(P2) ESTOQUE: O CNPJ "%s" digitado é inválido!', [FRegistroP1.CNPJ]);

     for intFor := 0 to FRegistroP2.Count - 1 do
     begin
        with FRegistroP2.Items[intFor] do
        begin
          ///
          Add( LFill('P2') +
               LFill(FRegistroP1.CNPJ, 14) +
               RFill(COD_MERC_SERV, 14) +
               RFill(CEST, 7) +
               RFill(NCM, 8) +
               RFill(DESC_MERC_SERV, 50) +
               RFill(UN_MED, 6, ifThen(RegistroValido, ' ', '?')) +
               RFill(IAT, 1) +
               RFill(IPPT, 1) +
               RFill(ST, 1) +
               LFill(ALIQ, 4) +
               LFill(VL_UNIT, 12, 2) );
        end;
        ///
        FRegistroP9.TOT_REG := FRegistroP9.TOT_REG + 1;
     end;
  end;
end;

procedure TPAF_P.WriteRegistroP9;
begin
   if Assigned(FRegistroP9) then
   begin
      with FRegistroP9 do
      begin
        Check(funChecaCNPJ(FRegistroP1.CNPJ),            '(P9) TOTALIZAÇÃO: O CNPJ "%s" digitado é inválido!', [FRegistroP1.CNPJ]);
        Check(funChecaIE(FRegistroP1.IE, FRegistroP1.UF), '(P9) TOTALIZAÇÃO: A Inscrição Estadual "%s" digitada é inválida!', [FRegistroP1.IE]);
        ///
        Add( LFill('P9') +
             LFill(FRegistroP1.CNPJ, 14) +
             RFill(FRegistroP1.IE, 14) +
             LFill(TOT_REG, 6, 0) ) ;
      end;
   end;
end;

end.
