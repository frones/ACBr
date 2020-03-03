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

unit ACBrPAF_H_Class;

interface

uses SysUtils, Classes, DateUtils, ACBrTXTClass,
     ACBrPAF_H;

type

  { TPAF_H }

  TPAF_H = class(TACBrTXTClass)
  private
    FRegistroH1: TRegistroH1;       /// FRegistroE1
    FRegistroH2: TRegistroH2List;   /// Lista de FRegistroE2
    FRegistroH9: TRegistroH9;       /// FRegistroE9

    procedure CriaRegistros;
    procedure LiberaRegistros;
  public
    constructor Create; /// Create
    destructor Destroy; override; /// Destroy
    procedure LimpaRegistros;

    procedure WriteRegistroH1;
    procedure WriteRegistroH2;
    procedure WriteRegistroH9;

    property RegistroH1: TRegistroH1 read FRegistroH1 write FRegistroH1;
    property RegistroH2: TRegistroH2List read FRegistroH2 write FRegistroH2;
    property RegistroH9: TRegistroH9 read FRegistroH9 write FRegistroH9;
  end;

implementation

uses ACBrTXTUtils, ACBrUtil;

{ TPAF_E }

constructor TPAF_H.Create;
begin
  inherited;
  CriaRegistros;
end;

procedure TPAF_H.CriaRegistros;
begin
  FRegistroH1 := TRegistroH1.Create;
  FRegistroH2 := TRegistroH2List.Create;
  FRegistroH9 := TRegistroH9.Create;

  FRegistroH9.TOT_REG := 0;
end;

destructor TPAF_H.Destroy;
begin
  LiberaRegistros;
  inherited;
end;

procedure TPAF_H.LiberaRegistros;
begin
  FRegistroH1.Free;
  FRegistroH2.Free;
  FRegistroH9.Free;
end;

procedure TPAF_H.LimpaRegistros;
begin
  /// Limpa os Registros
  LiberaRegistros;
  /// Recriar os Registros Limpos
  CriaRegistros;
end;

procedure TPAF_H.WriteRegistroH1;
begin
   if Assigned(FRegistroH1) then
   begin
      with FRegistroH1 do
      begin
        Check(funChecaCNPJ(CNPJ), '(H1) ESTABELECIMENTO: O CNPJ "%s" digitado é inválido!', [CNPJ]);
        Check(funChecaIE(IE, UF), '(H1) ESTABELECIMENTO: A Inscrição Estadual "%s" digitada é inválida!', [IE]);

        Add(LFill('H1') +
            LFill(OnlyNumber(CNPJ), 14) +
            RFill(IE, 14) +
            RFill(IM, 14) +
            RFill(RAZAOSOCIAL, 50, ifThen(not InclusaoExclusao, ' ', '?')) +
            RFill(NUM_FAB, 20) +
            RFill(MF_ADICIONAL, 1) +
            RFill(TIPO_ECF, 7) +
            RFill(MARCA_ECF, 20) +
            RFill(MODELO_ECF, 20,  ifThen(RegistroValido, ' ', '?')));
      end;
   end;
end;

function OrdenarH2(const ARegistro1, ARegistro2: Pointer): Integer;
var
  COO1, COO2: LongInt;
begin
  COO1 := TRegistroH2(ARegistro1).COO;
  COO2 := TRegistroH2(ARegistro2).COO;

  if COO1 < COO2 then
    Result := -1
  else
  if COO1 > COO2 then
    Result := 1
  else
    Result := 0;
end;

procedure TPAF_H.WriteRegistroH2;
var
intFor: integer;
begin
  if Assigned(FRegistroH2) then
  begin
    FRegistroH2.Sort(@OrdenarH2);

    if FRegistroH2.Count > 0 then
    begin
      with FRegistroH2.Items[0] do
      begin
        Check(funChecaCNPJ(FRegistroH1.CNPJ), '(H2) ESTOQUE: O CNPJ "%s" digitado é inválido!', [FRegistroH1.CNPJ]);
        Check(funChecaCNPJ(CNPJ), '(H2) RECEBEDORA DA DOAÇÃO: O CNPJ "%s" digitado é inválido!', [CNPJ]);
      end;
    end;

    for intFor := 0 to FRegistroH2.Count - 1 do
    begin
      with FRegistroH2.Items[intFor] do
      begin
        Add( LFill('H2') +
             LFill(OnlyNumber(CNPJ_CRED_CARTAO), 14) +
             RFill(NUM_FAB, 20) +
             RFill(MF_ADICIONAL, 1) +
             RFill(TIPO_ECF, 7) +
             RFill(MARCA_ECF, 20) +
             //RFill(MODELO_ECF, 20) +
             RFill(MODELO_ECF, 20,  ifThen(RegistroValido, ' ', '?')) +
             LFill(COO, 9) +
             LFill(CCF, 9) +
             LFill(VLR_TROCO, 13) +
             LFill(DT_TROCO, 'yyyymmdd') +
             LFill(OnlyNumber(CPF), 14) +
             LFill(Titulo, 7) +
             LFill(OnlyNumber(CNPJ), 14) );
      end;
      FRegistroH9.TOT_REG := FRegistroH9.TOT_REG + 1;
    end;
  end;
end;

procedure TPAF_H.WriteRegistroH9;
begin
   if Assigned(FRegistroH9) then
   begin
      with FRegistroH9 do
      begin
        Check(funChecaCNPJ(FRegistroH1.CNPJ),             '(H9) TOTALIZAÇÃO: O CNPJ "%s" digitado é inválido!', [FRegistroH1.CNPJ]);
        Check(funChecaIE(FRegistroH1.IE, FRegistroH1.UF), '(H9) TOTALIZAÇÃO: A Inscrição Estadual "%s" digitada é inválida!', [FRegistroH1.IE]);
        ///
        Add(LFill('H9') +
            LFill(OnlyNumber(FRegistroH1.CNPJ), 14) +
            RFill(FRegistroH1.IE, 14) +
            RFill(FRegistroH1.IM, 14) +
            LFill(TOT_REG, 6, 0));
      end;
   end;
end;

end.
