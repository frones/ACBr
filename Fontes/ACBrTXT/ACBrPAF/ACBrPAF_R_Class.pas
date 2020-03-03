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

unit ACBrPAF_R_Class;

interface

uses SysUtils, Classes, DateUtils, ACBrTXTClass,
     ACBrPAF_R, ACBrUtil;

type
  // TPAF_R -
  TPAF_R = class(TACBrTXTClass)
  private
    FRegistroR01: TRegistroR01List;   // RegistroR01

    FRegistroR03Count: Integer;
    FRegistroR05Count: Integer;
    FRegistroR07Count: Integer;

    procedure WriteRegistroR02(RegR01 :TRegistroR01);
    procedure WriteRegistroR03(RegR02: TRegistroR02;RegR01: TRegistroR01);
    procedure WriteRegistroR04(RegR01 :TRegistroR01);
    procedure WriteRegistroR05(RegR04: TRegistroR04;RegR01: TRegistroR01);
    procedure WriteRegistroR06(RegR01 :TRegistroR01);
    procedure WriteRegistroR07(RegR01 :TRegistroR01);

    procedure CriaRegistros;
    procedure LiberaRegistros;
  public
    constructor Create; // Create
    destructor Destroy; override; // Destroy
    procedure LimpaRegistros;

    procedure WriteRegistroR01;

    property RegistroR01: TRegistroR01List read FRegistroR01 write FRegistroR01;
    property RegistroR03Count: Integer read FRegistroR03Count write FRegistroR03Count;
    property RegistroR05Count: Integer read FRegistroR05Count write FRegistroR05Count;
    property RegistroR07Count: Integer read FRegistroR07Count write FRegistroR07Count;
  end;

implementation

uses Math, ACBrTXTUtils;

{ ordenações de registros }

function OrdenarR02(const ARegistro1, ARegistro2: Pointer): Integer;
var
  Reducao1, Reducao2: String;
begin
  Reducao1 :=
    Format('%2.2d', [TRegistroR02(ARegistro1).NUM_USU]) +
    Format('%6.6d', [TRegistroR02(ARegistro1).CRZ]) +
    Format('%6.6d', [TRegistroR02(ARegistro1).CRO]);

  Reducao2 :=
    Format('%2.2d', [TRegistroR02(ARegistro2).NUM_USU]) +
    Format('%6.6d', [TRegistroR02(ARegistro2).CRZ]) +
    Format('%6.6d', [TRegistroR02(ARegistro2).CRO]);

  Result := AnsiCompareText(Reducao1, Reducao2);
end;

function OrdenarR01(const ARegistro1, ARegistro2: Pointer): Integer;
var
  Ecf1, Ecf2: String;
begin
  Ecf1 := Format('%2.2d', [TRegistroR01(ARegistro1).NUM_SEQ_ECF]);


  Ecf2 := Format('%2.2d', [TRegistroR01(ARegistro2).NUM_SEQ_ECF]);


  Result := AnsiCompareText(Ecf1, Ecf2);
end;

function OrdenarR03(const ARegistro1, ARegistro2: Pointer): Integer;
var
  Detalhe1, Detalhe2: String;
begin
  Detalhe1 := Format('%-7s', [TRegistroR03(ARegistro1).TOT_PARCIAL]);
  Detalhe2 := Format('%-7s', [TRegistroR03(ARegistro2).TOT_PARCIAL]);

  Result := AnsiCompareText(Detalhe1, Detalhe2);
end;

function OrdenarR04(const ARegistro1, ARegistro2: Pointer): Integer;
var
  Cupom1, Cupom2: String;
begin
  Cupom1 :=
    Format('%2.2d', [TRegistroR04(ARegistro1).NUM_USU]) +
    Format('%6.6d', [TRegistroR04(ARegistro1).NUM_CONT]);

  Cupom2 :=
    Format('%2.2d', [TRegistroR04(ARegistro2).NUM_USU]) +
    Format('%6.6d', [TRegistroR04(ARegistro2).NUM_CONT]);

  Result := AnsiCompareText(Cupom1, Cupom2);
end;

function OrdenarR05(const ARegistro1, ARegistro2: Pointer): Integer;
var
  Detalhe1, Detalhe2: Integer;
begin
  Detalhe1 := TRegistroR05(ARegistro1).NUM_ITEM;
  Detalhe2 := TRegistroR05(ARegistro2).NUM_ITEM;

  if Detalhe1 < Detalhe2 then
    Result := -1
  else
  if Detalhe1 > Detalhe2 then
    Result := 1
  else
    Result := 0;
end;

function OrdenarR06(const ARegistro1, ARegistro2: Pointer): Integer;
var
  Doc1, Doc2: String;
begin
  Doc1 :=
    Format('%2.2d', [TRegistroR06(ARegistro1).NUM_USU]) +
    Format('%6.6d', [TRegistroR06(ARegistro1).COO]);

  Doc2 :=
    Format('%2.2d', [TRegistroR06(ARegistro2).NUM_USU]) +
    Format('%6.6d', [TRegistroR06(ARegistro2).COO]);

  Result := AnsiCompareText(Doc1, Doc2);
end;

function OrdenarR07(const ARegistro1, ARegistro2: Pointer): Integer;
var
  Pagto1, Pagto2: String;
begin

  if TRegistroR07(ARegistro1).TipoRegistroPai = 'R04' then
  begin
    Pagto1 :=
      Format('%2.2d', [TRegistroR04(TRegistroR07(ARegistro1).RegistroPai).NUM_USU]) +
      Format('%6.6d', [TRegistroR04(TRegistroR07(ARegistro1).RegistroPai).COO]) +
      Format('%6.6d', [TRegistroR07(ARegistro1).GNF]) +
      Format('%6.6d', [TRegistroR07(ARegistro1).CCF]) +
      Format('%-15s', [TRegistroR07(ARegistro1).MP]);
  end
  else if TRegistroR07(ARegistro1).TipoRegistroPai = 'R06' then
  begin
    Pagto1 :=
      Format('%2.2d', [TRegistroR06(TRegistroR07(ARegistro1).RegistroPai).NUM_USU]) +
      Format('%6.6d', [TRegistroR06(TRegistroR07(ARegistro1).RegistroPai).COO]) +
      Format('%6.6d', [TRegistroR07(ARegistro1).GNF]) +
      Format('%6.6d', [TRegistroR07(ARegistro1).CCF]) +
      Format('%-15s', [TRegistroR07(ARegistro1).MP]);
  end;

  if TRegistroR07(ARegistro2).TipoRegistroPai = 'R04' then
  begin
    Pagto2 :=
      Format('%2.2d', [TRegistroR04(TRegistroR07(ARegistro2).RegistroPai).NUM_USU]) +
      Format('%6.6d', [TRegistroR04(TRegistroR07(ARegistro2).RegistroPai).COO]) +
      Format('%6.6d', [TRegistroR07(ARegistro2).GNF]) +
      Format('%6.6d', [TRegistroR07(ARegistro2).CCF]) +
      Format('%-15s', [TRegistroR07(ARegistro2).MP]);
  end
  else if TRegistroR07(ARegistro2).TipoRegistroPai = 'R06' then
  begin
    Pagto2 :=
      Format('%2.2d', [TRegistroR06(TRegistroR07(ARegistro2).RegistroPai).NUM_USU]) +
      Format('%6.6d', [TRegistroR06(TRegistroR07(ARegistro2).RegistroPai).COO]) +
      Format('%6.6d', [TRegistroR07(ARegistro2).GNF]) +
      Format('%6.6d', [TRegistroR07(ARegistro2).CCF]) +
      Format('%-15s', [TRegistroR07(ARegistro2).MP]);
  end;

  Result := AnsiCompareText(Pagto1, Pagto2);
end;

{ TPAF_R }

constructor TPAF_R.Create;
begin
  inherited;
  CriaRegistros;
end;

destructor TPAF_R.Destroy;
begin
  LiberaRegistros;
  inherited;
end;

procedure TPAF_R.CriaRegistros;
begin
  FRegistroR01 := TRegistroR01List.Create;

  FRegistroR03Count := 0;
  FRegistroR05Count := 0;
  FRegistroR07Count := 0;
end;

procedure TPAF_R.LiberaRegistros;
begin
  FRegistroR01.Free;
end;

procedure TPAF_R.LimpaRegistros;
begin
  // Limpa os Registros
  LiberaRegistros;
  // Recriar os Registros Limpos
  CriaRegistros;
end;

procedure TPAF_R.WriteRegistroR01;
var
  intFor,intFor2: integer;
begin
  if Assigned(FRegistroR01) then
  begin
    FRegistroR01.Sort(@OrdenarR01);

    if FRegistroR01.Count > 0 then
    begin
      with FRegistroR01.Items[0] do
      begin
        Check(funChecaCNPJ(CNPJ), '(R01) ESTABELECIMENTO: O CNPJ "%s" digitado é inválido!', [CNPJ]);
        Check(funChecaCNPJ(CNPJ_SH), '(R01) SOFTHOUSE: O CNPJ "%s" digitado é inválido!', [CNPJ_SH]);
      end;
    end;

    for intFor := 0 to FRegistroR01.Count - 1 do
    begin
      with FRegistroR01.Items[intFor] do
      begin
        Add(LFill('R01') +
            RFill(NUM_FAB, 20) +
            RFill(MF_ADICIONAL, 1) +
            RFill(TIPO_ECF, 7) +
            RFill(MARCA_ECF, 20) +
            RFill(MODELO_ECF, 20, ifThen(RegistroValido, ' ', '?')) +
            RFill(VERSAO_SB, 10) +
            LFill(DT_INST_SB, 'yyyymmdd') +
            LFill(HR_INST_SB, 'hhmmss') +
            LFill(NUM_SEQ_ECF, 3) +
            LFill(CNPJ, 14) +
            RFill(IE, 14) +
            LFill(CNPJ_SH, 14) +
            RFill(IE_SH, 14) +
            RFill(IM_SH, 14) +
            RFill(NOME_SH, 40, ifThen(not InclusaoExclusao, ' ', '?')) +
            RFill(NOME_PAF, 40) + 
            RFill(VER_PAF, 10) +
            RFill(COD_MD5, 32) +
            LFill(DT_INI, 'yyyymmdd') +
            LFill(DT_FIN, 'yyyymmdd') +
            RFill(ER_PAF_ECF, 4));
      end;
    end;

    for intFor := 0 to FRegistroR01.Count - 1 do
      WriteRegistroR02(FRegistroR01.Items[intFor]);

    for intFor := 0 to FRegistroR01.Count - 1 do
      for intFor2 := 0 to FRegistroR01.Items[intFor].RegistroR02.Count-1 do
        WriteRegistroR03( FRegistroR01.Items[intFor].RegistroR02.Items[intFor2], FRegistroR01.Items[intFor]);

    for intFor := 0 to FRegistroR01.Count - 1 do
      WriteRegistroR04(FRegistroR01.Items[intFor]);

    for intFor := 0 to FRegistroR01.Count - 1 do
      for intFor2 := 0 to FRegistroR01.Items[intFor].RegistroR04.Count-1 do
        WriteRegistroR05( FRegistroR01.Items[intFor].RegistroR04.Items[intFor2], FRegistroR01.Items[intFor]);

    for intFor := 0 to FRegistroR01.Count - 1 do
      WriteRegistroR06(FRegistroR01.Items[intFor]);

    for intFor := 0 to FRegistroR01.Count - 1 do
      WriteRegistroR07(FRegistroR01.Items[intFor]);

  end;
end;

procedure TPAF_R.WriteRegistroR02(RegR01: TRegistroR01);
var
  intFor: integer;
begin
  if Assigned(RegR01.RegistroR02) then
  begin
     RegR01.RegistroR02.Sort(@OrdenarR02);

     for intFor := 0 to RegR01.RegistroR02.Count - 1 do
     begin
        with RegR01.RegistroR02.Items[intFor] do
        begin
          Add( LFill('R02') +
               RFill(RegR01.NUM_FAB, 20) +
               RFill(RegR01.MF_ADICIONAL, 1) +
               RFill(RegR01.MODELO_ECF, 20, ifThen(RegistroValido, ' ', '?')) +
               LFill(NUM_USU, 2) +
               LFill(CRZ, 6) +
               LFill(COO, 9) +
               LFill(CRO, 6) +
               LFill(DT_MOV, 'yyyymmdd') +
               LFill(DT_EMI, 'yyyymmdd') +
               LFill(HR_EMI, 'hhmmss', False) +
               LFill(VL_VBD, 14, 2) +
               RFill(PAR_ECF, 1));
        end;
     end;
  end;
end;

procedure TPAF_R.WriteRegistroR03(RegR02: TRegistroR02; RegR01: TRegistroR01);
var
  intFor: integer;
begin
  if Assigned(RegR02.RegistroR03) then
  begin
     RegR02.RegistroR03.Sort(@OrdenarR03);

     for intFor := 0 to RegR02.RegistroR03.Count - 1 do
     begin
        with RegR02.RegistroR03.Items[intFor] do
        begin
          Add( LFill('R03') +
               RFill(RegR01.NUM_FAB, 20) +
               RFill(RegR01.MF_ADICIONAL, 1) +
               RFill(RegR01.MODELO_ECF, 20, ifThen(RegistroValido, ' ', '?')) +
               LFill(RegR02.NUM_USU, 2) +
               LFill(RegR02.CRZ, 6) +
               RFill(TOT_PARCIAL, 7) +
               LFill(VL_ACUM, 13, 2));
        end;
     end;
     // Variavél para armazenar a quantidade de registro do tipo.
     FRegistroR03Count := FRegistroR03Count + RegR02.RegistroR03.Count;
  end;
end;

procedure TPAF_R.WriteRegistroR04(RegR01: TRegistroR01);
var
  intFor: integer;
begin
  if Assigned(RegR01.RegistroR04) then
  begin
     RegR01.RegistroR04.Sort(@OrdenarR04);

     for intFor := 0 to RegR01.RegistroR04.Count - 1 do
     begin
        with RegR01.RegistroR04.Items[intFor] do
        begin
          Add( LFill('R04') +
               RFill(RegR01.NUM_FAB, 20) +
               RFill(RegR01.MF_ADICIONAL, 1) +
               RFill(RegR01.MODELO_ECF, 20, ifThen(RegistroValido, ' ', '?')) +
               LFill(NUM_USU, 2) +
               LFill(NUM_CONT, 9) +
               LFill(COO, 9) +
               LFill(DT_INI, 'yyyymmdd') +
               LFill(SUB_DOCTO, 14, 2) +
               LFill(SUB_DESCTO, 13, 2) +
               RFill(TP_DESCTO, 1) +
               LFill(SUB_ACRES, 13, 2) +
               RFill(TP_ACRES, 1) +
               LFill(VL_TOT, 14, 2) +
               RFill(CANC, 1) +
               LFill(VL_CA, 13, 2) +
               RFill(ORDEM_DA, 1) +
               RFill(NOME_CLI, 40) +
               LFill(CNPJ_CPF, 14));
        end;
     end;
  end;
end;

procedure TPAF_R.WriteRegistroR05(RegR04: TRegistroR04; RegR01: TRegistroR01);
var
  intFor: integer;
begin
  if Assigned(RegR04.RegistroR05) then
  begin
     RegR04.RegistroR05.Sort(@OrdenarR05);

     for intFor := 0 to RegR04.RegistroR05.Count - 1 do
     begin
        with RegR04.RegistroR05.Items[intFor] do
        begin
          Add( LFill('R05') +
               RFill(RegR01.NUM_FAB, 20) +
               RFill(RegR01.MF_ADICIONAL, 1) +
               RFill(RegR01.MODELO_ECF, 20, ifThen(RegistroValido, ' ', '?')) +
               LFill(RegR04.NUM_USU, 2) +
               LFill(RegR04.COO, 9) +
               LFill( Math.IfThen(NUM_CONT=-1, RegR04.NUM_CONT, NUM_CONT), 9) +
               LFill(NUM_ITEM, 3) +
               RFill(COD_ITEM, 14) +
               RFill(DESC_ITEM, 100) +
               LFill(QTDE_ITEM, 7, QTDE_DECIMAL) +
               RFill(UN_MED, 3) +
               LFill(VL_UNIT, 8, VL_DECIMAL) +
               LFill(DESCTO_ITEM, 8, 2) +
               LFill(ACRES_ITEM, 8, 2) +
               LFill(VL_TOT_ITEM, 14, 2) +
               RFill(COD_TOT_PARC, 7) +
               RFill(IND_CANC, 1) +
               LFill(QTDE_CANC, 7, 2) +
               LFill(VL_CANC, 13, 2) +
               LFill(VL_CANC_ACRES, 13, 2) +
               RFill(IAT, 1) +
               RFill(IPPT, 1) +
               LFill(QTDE_DECIMAL, 1) +
               LFill(VL_DECIMAL, 1));
        end;
     end;
     // Variavél para armazenar a quantidade de registro do tipo.
     FRegistroR05Count := FRegistroR05Count + RegR04.RegistroR05.Count;
  end;
end;

procedure TPAF_R.WriteRegistroR06(RegR01: TRegistroR01);
var
  intFor: integer;
begin
  if Assigned(RegR01.RegistroR06) then
  begin
     RegR01.RegistroR06.Sort(@OrdenarR06);

     for intFor := 0 to RegR01.RegistroR06.Count - 1 do
     begin
        with RegR01.RegistroR06.Items[intFor] do
        begin
          Add( LFill('R06') +
               RFill(RegR01.NUM_FAB, 20) +
               RFill(RegR01.MF_ADICIONAL, 1) +
               RFill(RegR01.MODELO_ECF, 20, ifThen(RegistroValido, ' ', '?')) +
               LFill(NUM_USU, 2) +
               LFill(COO, 9) +
               LFill(GNF, 6) +
               LFill(GRG, 6) +
               LFill(CDC, 4) +
               RFill(DENOM, 2) +
               LFill(DT_FIN, 'yyyymmdd') +
               LFill(HR_FIN, 'hhmmss'));
        end;
     end;
  end;
end;

procedure TPAF_R.WriteRegistroR07(RegR01: TRegistroR01);
var
  intForPai, intForFilho: Integer;
  FRegistroR07x: TRegistroR07xList;   // Lista de RegistroR07 para ordenação
begin
  { TODO : Percorrer os Registros R07 e reordená-los, retornando o resultado da montagem de todos os registros. }
  FRegistroR07x := TRegistroR07xList.Create;;
  try
    FRegistroR07x.OwnsObjects := False; //Os objetos adicionados nessa lista não podem ser destruídos por ela.

    //Adiciona os R07 que são itens do R04 na lista
    for intForPai := 0 to RegR01.RegistroR04.Count - 1 do
    begin
      if Assigned(RegR01.RegistroR04.Items[intForPai].RegistroR07) then
      begin
        for intForFilho := 0 to RegR01.RegistroR04.Items[intForPai].RegistroR07.Count - 1 do
        begin
          FRegistroR07x.Add( RegR01.RegistroR04.Items[intForPai].RegistroR07.Items[intForFilho]);
        end;
      end;
    end;

    //Adiciona os R07 que são itens do R06 na lista
    for intForPai := 0 to RegR01.RegistroR06.Count - 1 do
    begin
      if Assigned(RegR01.RegistroR06.Items[intForPai].RegistroR07) then
      begin
        for intForFilho := 0 to RegR01.RegistroR06.Items[intForPai].RegistroR07.Count - 1 do
        begin
          FRegistroR07x.Add( RegR01.RegistroR06.Items[intForPai].RegistroR07.Items[intForFilho]);
        end;
      end;
    end;

    //Faz ordenação
    FRegistroR07x.Sort(@OrdenarR07);

    for intForPai := 0 to FRegistroR07x.Count - 1 do
    begin
      with FRegistroR07x.Items[intForPai] do
      begin
        if TipoRegistroPai = 'R04' then
        begin
          Add( LFill('R07') +
               RFill(RegR01.NUM_FAB, 20) +
               RFill(RegR01.MF_ADICIONAL, 1) +
               RFill(RegR01.MODELO_ECF, 20, ifThen(RegistroValido, ' ', '?')) +
               LFill(TRegistroR04(RegistroPai).NUM_USU, 2) +
               LFill(COO, 9) +
               LFill(CCF, 9) +
               LFill(GNF, 6) +
               RFill(MP, 15) +
               LFill(VL_PAGTO, 13, 2) +
               RFill(IND_EST, 1) +
               LFill(VL_EST, 13, 2));
        end
        else if TipoRegistroPai = 'R06' then
        begin
          Add( LFill('R07') +
               RFill(RegR01.NUM_FAB, 20) +
               RFill(RegR01.MF_ADICIONAL, 1) +
               RFill(RegR01.MODELO_ECF, 20, ifThen(RegistroValido, ' ', '?')) +
               LFill(TRegistroR06(RegistroPai).NUM_USU, 2) +
               LFill(COO, 9) +
               LFill(CCF, 9) +
               LFill(GNF, 6) +
               RFill(MP, 15) +
               LFill(VL_PAGTO, 13, 2) +
               RFill(IND_EST, 1) +
               LFill(VL_EST, 13, 2));
        end;
      end;
    end;
  finally
    FreeAndNil(FRegistroR07x);
  end;
end;

end.
