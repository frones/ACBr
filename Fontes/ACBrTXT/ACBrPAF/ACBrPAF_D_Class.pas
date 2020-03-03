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

unit ACBrPAF_D_Class;

interface

uses SysUtils, Classes, DateUtils, ACBrTXTClass,
     ACBrPAF_D;

type
  /// TACBrPAF_D -

  { TPAF_D }

  TPAF_D = class(TACBrTXTClass)
  private
    FRegistroD1: TRegistroD1;       /// FRegistroD1
    FRegistroD2: TRegistroD2List;   /// Lista de FRegistroD2
    FRegistroD9: TRegistroD9;       /// FRegistroD9

    procedure WriteRegistroD3(RegD2: TRegistroD2);
    procedure WriteRegistroD4(RegD2: TRegistroD2);

    procedure CriaRegistros;
    procedure LiberaRegistros;
  public
    constructor Create; /// Create
    destructor Destroy; override; /// Destroy
    procedure LimpaRegistros;

    procedure WriteRegistroD1;
    procedure WriteRegistroD2;
    procedure WriteRegistroD9;

    property RegistroD1: TRegistroD1     read FRegistroD1 write FRegistroD1;
    property RegistroD2: TRegistroD2List read FRegistroD2 write FRegistroD2;
    property RegistroD9: TRegistroD9     read FRegistroD9 write FRegistroD9;
  end;

implementation

uses ACBrTXTUtils;

{ TPAF_D }

constructor TPAF_D.Create;
begin
  inherited;
  CriaRegistros;
end;

procedure TPAF_D.CriaRegistros;
begin
  FRegistroD1  := TRegistroD1.Create;
  FRegistroD2  := TRegistroD2List.Create;
  FRegistroD9  := TRegistroD9.Create;

  FRegistroD9.TOT_REG_D2 := 0;
  FRegistroD9.TOT_REG_D3 := 0;
  FRegistroD9.TOT_REG    := 0;
end;

destructor TPAF_D.Destroy;
begin
  LiberaRegistros;
  inherited;
end;

procedure TPAF_D.LiberaRegistros;
begin
  FRegistroD1.Free;
  FRegistroD2.Free;
  FRegistroD9.Free;
end;

procedure TPAF_D.LimpaRegistros;
begin
  /// Limpa os Registros
  LiberaRegistros;
  /// Recriar os Registros Limpos
  CriaRegistros;
end;

procedure TPAF_D.WriteRegistroD1;
begin
   if Assigned(FRegistroD1) then
   begin
      with FRegistroD1 do
      begin
        Check(funChecaCNPJ(CNPJ), '(D1) ESTABELECIMENTO: O CNPJ "%s" digitado é inválido!', [CNPJ]);
        Check(funChecaIE(IE, UF), '(D1) ESTABELECIMENTO: A Inscrição Estadual "%s" digitada é inválida!', [IE]);
        ///
        Add( LFill('D1') +
             LFill(CNPJ, 14) +
             RFill(IE, 14) +
             RFill(IM, 14) +
             RFill(RAZAOSOCIAL, 50, ifThen(not InclusaoExclusao, ' ', '?')) );
      end;
   end;
end;

function OrdenarD2(const ARegistro1, ARegistro2: Pointer): Integer;
var
  Dav1, Dav2: LongInt;
begin
  Dav1 := StrToIntDef(TRegistroD2(ARegistro1).NUM_DAV, 0);
  Dav2 := StrToIntDef(TRegistroD2(ARegistro2).NUM_DAV, 0);

  if Dav1 < Dav2 then
    Result := -1
  else
  if Dav1 > Dav2 then
    Result := 1
  else
    Result := 0;
end;

procedure TPAF_D.WriteRegistroD2;
var
  intFor: integer;
begin
  if Assigned(FRegistroD2) then
  begin
    FRegistroD2.Sort(@OrdenarD2);

    Check(funChecaCNPJ(FRegistroD1.CNPJ), '(D2) DAV EMITIDOS: O CNPJ "%s" digitado é inválido!', [FRegistroD1.CNPJ]);
    for intFor := 0 to FRegistroD2.Count - 1 do
    begin
      with FRegistroD2.Items[intFor] do
      begin
        Add( LFill('D2') +
             LFill(FRegistroD1.CNPJ, 14) +
             RFill(NUM_FAB, 20) +
             RFill(MF_ADICIONAL, 1) +
             RFill(TIPO_ECF, 7) +
             RFill(MARCA_ECF, 20) +
             RFill(MODELO_ECF, 20, ifThen(RegistroValido, ' ', '?')) +
             LFill(COO, 9) +
             RFill(NUM_DAV, 13) +
             LFill(DT_DAV, 'yyyymmdd') +
             RFill(TIT_DAV, 30) +
             LFill(VLT_DAV, 8, 2) +
             LFill(COO_DFV, 9) +
             LFill(NUMERO_ECF, 3) +
             RFill(NOME_CLIENTE, 40) +
             LFill(CPF_CNPJ, 14) );
      end;
      FRegistroD9.TOT_REG_D2 := FRegistroD9.TOT_REG_D2 + 1;
      FRegistroD9.TOT_REG    := FRegistroD9.TOT_REG + 1;
    end;

    // Registro FILHOS
    for intFor := 0 to FRegistroD2.Count - 1 do
      WriteRegistroD3( FRegistroD2.Items[intFor] );

    for intFor := 0 to FRegistroD2.Count - 1 do
      WriteRegistroD4( FRegistroD2.Items[intFor] );

  end;
end;

function OrdenarD3(const ARegistro1, ARegistro2: Pointer): Integer;
var
  Item1, Item2: LongInt;
begin
  Item1 := TRegistroD3(ARegistro1).NUM_ITEM;
  Item2 := TRegistroD3(ARegistro2).NUM_ITEM;

  if Item1 < Item2 then
    Result := -1
  else
  if Item1 > Item2 then
    Result := 1
  else
    Result := 0;
end;

procedure TPAF_D.WriteRegistroD3(RegD2: TRegistroD2);
var
intFor: integer;
begin
  if Assigned(RegD2.RegistroD3) then
  begin
    RegD2.RegistroD3.Sort(@OrdenarD3);
     for intFor := 0 to RegD2.RegistroD3.Count - 1 do
     begin
        with RegD2.RegistroD3.Items[intFor] do
        begin
          ///
          Add( LFill('D3') +
               RFill(RegD2.NUM_DAV, 13) +
               LFill(DT_INCLUSAO, 'yyyymmdd') +
               LFill(NUM_ITEM, 3, 0) +
               RFill(COD_ITEM, 14) +
               RFill(DESC_ITEM, 100, ifThen(RegistroValido, ' ', '?')) +
               LFill(QTDE_ITEM, 7, DEC_QTDE_ITEM) +
               RFill(UNI_ITEM, 3) +
               LFill(VL_UNIT, 8, DEC_VL_UNIT) +
               LFill(VL_DESCTO, 8, 2) +
               LFill(VL_ACRES, 8, 2) +
               LFill(VL_TOTAL, 14, 2) +
               RFill(SIT_TRIB, 1) +
               LFill(ALIQ, 4, 2) +
               LFill(IND_CANC) +
               LFill(DEC_QTDE_ITEM, 1) +
               LFill(DEC_VL_UNIT, 1) );
        end;
        ///
        FRegistroD9.TOT_REG_D3 := FRegistroD9.TOT_REG_D3 + 1;
        FRegistroD9.TOT_REG    := FRegistroD9.TOT_REG + 1;
     end;
  end;
end;

function OrdenarD4(const ARegistro1, ARegistro2: Pointer): Integer;
var
  Reg1, Reg2: String;
begin
  Reg1 :=
    Format('%-13s', [TRegistroD4(ARegistro1).NUM_DAV]) +
    FormatDateTime('yyyymmddhhmmss', TRegistroD4(ARegistro1).DT_ALT);

  Reg2 :=
    Format('%-13s', [TRegistroD4(ARegistro2).NUM_DAV]) +
    FormatDateTime('yyyymmddhhmmss', TRegistroD4(ARegistro2).DT_ALT);

  Result := AnsiCompareText(Reg1, Reg2);
end;

procedure TPAF_D.WriteRegistroD4(RegD2: TRegistroD2);
var
  intFor: integer;
begin
  if Assigned(RegD2.RegistroD4) then
  begin
    RegD2.RegistroD4.Sort(@OrdenarD4);
    
    for intFor := 0 to RegD2.RegistroD4.Count - 1 do
    begin
      with RegD2.RegistroD4.Items[intFor] do
      begin
        Add( LFill('D4') +
             RFill(NUM_DAV, 13) +
             LFill(DT_ALT, 'yyyymmddhhmmss') +
             RFill(COD_ITEM, 14) +
             RFill(DESC_ITEM, 100, ifThen(RegistroValido, ' ', '?')) +
             LFill(QTDE_ITEM, 7, DEC_QTDE_ITEM) +
             RFill(UNI_ITEM, 3) +
             LFill(VL_UNIT, 8, DEC_VL_UNIT) +
             LFill(VL_DESCTO, 8, 2) +
             LFill(VL_ACRES, 8, 2) +
             LFill(VL_TOTAL, 14, 2) +
             RFill(SIT_TRIB, 1) +
             LFill(ALIQ, 4, 2) +
             LFill(IND_CANC) +
             LFill(DEC_QTDE_ITEM, 1) +
             LFill(DEC_VL_UNIT, 1) +
             RFill(TIP_ALT, 1) );
      end;
    end;
  end;
end;

procedure TPAF_D.WriteRegistroD9;
begin
   if Assigned(FRegistroD9) then
   begin
      with FRegistroD9 do
      begin
        Check(funChecaCNPJ(FRegistroD1.CNPJ),             '(D9) TOTALIZAÇÃO: O CNPJ "%s" digitado é inválido!', [FRegistroD1.CNPJ]);
        Check(funChecaIE(FRegistroD1.IE, FRegistroD1.UF), '(D9) TOTALIZAÇÃO: A Inscrição Estadual "%s" digitada é inválida!', [FRegistroD1.IE]);
        ///
        Add(LFill('D9') +
            LFill(FRegistroD1.CNPJ, 14) +
            RFill(FRegistroD1.IE, 14) +
            LFill(TOT_REG_D2, 6, 0) +
            LFill(TOT_REG_D3, 6, 0));
      end;
   end;
end;

end.
