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

unit ACBrPAF_F_Class;

interface

uses SysUtils, Classes, DateUtils, ACBrTXTClass, ACBrPAF_F;

type

  { TPAF_F }

  TPAF_F = class(TACBrTXTClass)
  private
    FRegistroF2: TRegistroF2List;   // Lista de FRegistroF2
    FRegistroF3: TRegistroF3List;   // Lista de FRegistroF3
    FRegistroF4: TRegistroF4List;   // Lista de FRegistroF4

    procedure CriaRegistros;
    procedure LiberaRegistros;

  public
    constructor Create;             // Create
    destructor Destroy; override;   // Destroy
    procedure LimpaRegistros;

    procedure WriteRegistroF2;
    procedure WriteRegistroF3;
    procedure WriteRegistroF4;

    property RegistroF2: TRegistroF2List read FRegistroF2 write FRegistroF2;
    property RegistroF3: TRegistroF3List read FRegistroF3 write FRegistroF3;
    property RegistroF4: TRegistroF4List read FRegistroF4 write FRegistroF4;
  end;

implementation

uses ACBrTXTUtils;

{ TPAF_F }
constructor TPAF_F.Create;
begin
  inherited;
  CriaRegistros;
end;

procedure TPAF_F.CriaRegistros;
begin
  FRegistroF2 := TRegistroF2List.Create;
  FRegistroF3 := TRegistroF3List.Create;
  FRegistroF4 := TRegistroF4List.Create;
end;

destructor TPAF_F.Destroy;
begin
  LiberaRegistros;
  inherited;
end;

procedure TPAF_F.LiberaRegistros;
begin
  FRegistroF2.Free;
  FRegistroF3.Free;
  FRegistroF4.Free;
end;

procedure TPAF_F.LimpaRegistros;
begin
  /// Limpa os Registros
  LiberaRegistros;
  /// Recriar os Registros Limpos
  CriaRegistros;
end;

function OrdenarF2(const ARegistro1, ARegistro2: Pointer): Integer;
var
  Reg1, Reg2: String;
begin
  Reg1 :=
    TRegistroF2(ARegistro1).CNPJ_EMP +
    Format('%-20s', [TRegistroF2(ARegistro1).COD_LOCAL]) +
    Format('%-8s', [TRegistroF2(ARegistro1).ID_LINHA]) +
    FormatDateTime('yyyymmddhhmmss', TRegistroF2(ARegistro1).DT_PART);

  Reg2 :=
    TRegistroF2(ARegistro2).CNPJ_EMP +
    Format('%-20s', [TRegistroF2(ARegistro2).COD_LOCAL]) +
    Format('%-8s', [TRegistroF2(ARegistro2).ID_LINHA]) +
    FormatDateTime('yyyymmddhhmmss', TRegistroF2(ARegistro2).DT_PART);

  Result := AnsiCompareText(Reg1, Reg2);
end;

procedure TPAF_F.WriteRegistroF2;
var
  intFor: integer;
begin
  if Assigned(FRegistroF2) then
  begin
    FRegistroF2.Sort(@OrdenarF2);

    if FRegistroF2.Count > 0 then
    begin
      with FRegistroF2.Items[0] do
      begin
        Check(funChecaCNPJ(CNPJ_EMP), '(F2) MANIFESTO VIAGEM (CPNJ Empresa): O CNPJ "%s" digitado é inválido!', [CNPJ_EMP]);
        Check(funChecaCNPJ(CNPJ_ORG), '(F2) MANIFESTO VIAGEM (CPNJ Órgão): O CNPJ "%s" digitado é inválido!', [CNPJ_ORG]);
      end;
    end;

    for intFor := 0 to FRegistroF2.Count - 1 do
    begin
      with FRegistroF2.Items[intFor] do
      begin
      Add( LFill('F2') +
           LFill(CNPJ_ORG, 14) +
           LFill(CNPJ_EMP, 14) +
           LFill(COD_LOCAL, 20) +
           LFill(ID_LINHA, 8) +
           RFill(DESC_LINHA, 80, ifThen(RegistroValido, ' ', '?')) +
           LFill(DT_PART, 'yyyymmddhhmmss') +
           LFill(COD_VIAGEM, 2));
      end;
    end;
  end;
end;

function OrdenarF3(const ARegistro1, ARegistro2: Pointer): Integer;
var
  Reg1, Reg2: String;
begin
  Reg1 :=
    TRegistroF3(ARegistro1).NUM_FAB +
    Format('%6.6d', [TRegistroF3(ARegistro1).CCF]) +
    Format('%6.6d', [TRegistroF3(ARegistro1).COO]);

  Reg2 :=
    TRegistroF3(ARegistro2).NUM_FAB +
    Format('%6.6d', [TRegistroF3(ARegistro2).CCF]) +
    Format('%6.6d', [TRegistroF3(ARegistro2).COO]);

  Result := AnsiCompareText(Reg1, Reg2);
end;

procedure TPAF_F.WriteRegistroF3;
var
  intFor: integer;
begin
  if Assigned(RegistroF3) then
  begin
    RegistroF3.Sort(@OrdenarF3);
    for intFor := 0 to RegistroF3.Count - 1 do
    begin
      with RegistroF3.Items[intFor] do
      begin
        Add( LFill('F3') +
             RFill(NUM_FAB, 20) +
             RFill(MF_ADICIONAL, 1) +
             RFill(MODELO_ECF, 20) +
             LFill(NUM_USU, 2, False, IfThen(RegistroValido, ' ', '?') )+
             LFill(CCF, 6) +
             LFill(COO, 6) +
             LFill(COD_ORIG, 20) +
             LFill(COD_DEST, 20) +
             LFill(VL_DOC, 14, 2) +
             RFill(ST, 1) +
             LFill(COD_TSER, 2) +
             LFill(POLTRONA, 2));
      end;
    end;
  end;
end;

function OrdenarF4(const ARegistro1, ARegistro2: Pointer): Integer;
var
  Reg1, Reg2: String;
begin
  Reg1 := TRegistroF4(ARegistro1).COD_TSER;
  Reg2 := TRegistroF4(ARegistro2).COD_TSER;

  Result := AnsiCompareText(Reg1, Reg2);
end;

procedure TPAF_F.WriteRegistroF4;
var
  intFor: integer;
begin
  if Assigned(RegistroF4) then
  begin
    RegistroF4.Sort(@OrdenarF4);

    for intFor := 0 to RegistroF4.Count - 1 do
    begin
      with RegistroF4.Items[intFor] do
      begin
        Add( LFill('F4') +
             LFill(COD_TSER, 2, False, IfThen(RegistroValido, ' ', '?')) +
             LFill(QTDE_TOTAL, 4));
      end;
    end;
  end;

end;

end.
