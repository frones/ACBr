{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }

{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }

{ Colaboradores nesse arquivo: Ribamar M. Santos                               }
{                              Juliomar Marchetti                              }

{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }

{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }

{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }

{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }

{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

{$I ACBr.inc}

unit ACBrADRCST_Bloco1_Class;

interface

uses
  SysUtils,
  Classes,
  DateUtils,
  ACBrADRCST_Bloco1,
  ACBrTXTClass,
  ACBrADRCSTConversao;

type
  { TBloco_1}
  TBloco_1 = class(TACBrTXTClass)
  private
    FLayout : TADRCSTLayout;
    FRegistro1000List: TRegistro1000List;
    FRegistro1001List: TRegistro1001List;
    FRegistro1999: TRegistro1999;

    procedure WriteRegistro1010(const ARegistro1000: TRegistro1000);
    procedure WriteRegistro1100(const ARegistro1000: TRegistro1000);
    procedure WriteRegistro1101(const ARegistro1001: TRegistro1001);
    procedure WriteRegistro1110(const ARegistro1100: TRegistro1100);
    procedure WriteRegistro1111(const ARegistro1101: TRegistro1101);
    procedure WriteRegistro1120(const ARegistro1100: TRegistro1100);

    procedure WriteRegistro1200(const ARegistro1000: TRegistro1000);
    procedure WriteRegistro1210(const ARegistro1200: TRegistro1200);
    procedure WriteRegistro1220(const ARegistro1200: TRegistro1200);

    procedure WriteRegistro1300(const ARegistro1000: TRegistro1000);
    procedure WriteRegistro1310(const ARegistro1300: TRegistro1300);
    procedure WriteRegistro1320(const ARegistro1300: TRegistro1300);

    procedure WriteRegistro1400(const ARegistro1000: TRegistro1000);
    procedure WriteRegistro1410(const ARegistro1400: TRegistro1400);
    procedure WriteRegistro1420(const ARegistro1400: TRegistro1400);

    procedure WriteRegistro1500(const ARegistro1000: TRegistro1000);
    procedure WriteRegistro1510(const ARegistro1500: TRegistro1500);
    procedure WriteRegistro1520(const ARegistro1500: TRegistro1500);

    procedure WriteRegistro1999(const ARegistro1999: TRegistro1999);

    function GetLayout: TADRCSTLayout;
    procedure SetLayout(const Value: TADRCSTLayout);
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function Registro1000New: TRegistro1000;
    function Registro1001New: TRegistro1001;

    procedure WriteRegistro1000;
    procedure WriteRegistro1001;

    property Registro1000List: TRegistro1000List read FRegistro1000List write FRegistro1000List;
    property Registro1001List: TRegistro1001List read FRegistro1001List write FRegistro1001List;

    property Registro1999  : TRegistro1999 read FRegistro1999;
    property Layout : TADRCSTLayout read GetLayout write SetLayout;
  end;



implementation

{ TBloco_1 }

procedure TBloco_1.WriteRegistro1010(const ARegistro1000: TRegistro1000);
begin
  if Assigned(ARegistro1000.Registro1010) then
  begin
    with ARegistro1000.Registro1010 do
    begin
      Add(
        LFill(REG) +
        LFill(COD_ITEM)+
        LFill(UNID_ITEM)+
        LFill(QTD,9,3)+
        LFill(VL_TOT_ITEM,9,2)+
        LFill(TXT_COMPL),
        False
        );
    end;
  end;
end;

procedure TBloco_1.WriteRegistro1100(const ARegistro1000: TRegistro1000);
begin
  if Assigned(ARegistro1000.Registro1100) then
  begin
    with ARegistro1000.Registro1100 do
    begin
      Add(
        LFill(REG) +
        LFill(QTD_TOT_ENTRADA,9,3)+
        LFill(MENOR_VL_UNIT_ITEM,9,2)+
        LFill(VL_BC_ICMSST_UNIT_MED,9,2)+
        LFill(VL_TOT_ICMS_SUPORT_ENTR,9,2)+
        LFill(VL_UNIT_MED_ICMS_SUPORT_ENTR,9,2),
        False
        );
      WriteRegistro1110(ARegistro1000.Registro1100);
      WriteRegistro1120(ARegistro1000.Registro1100);
    end;
  end
  else
    raise Exception.Create('Registro 1100 é obrigatório!');
end;

procedure TBloco_1.WriteRegistro1101(const ARegistro1001: TRegistro1001);
begin
  if Assigned(ARegistro1001.Registro1101) then
  begin
    with ARegistro1001.Registro1101 do
    begin
      Add(
        LFill(REG) +
        LFill(QTD_TOT_ENTRADA,9,3)+
        LFill(VL_BC_ICMSST_UNIT_MED,9,2)+
        LFill(VL_TOT_ICMS_SUPORT_ENTR,9,2)+
        LFill(VL_UNIT_MED_ICMS_SUPORT_ENTR,9,2)+
        LFill(QTD_TRANF,9),
        False
        );
      WriteRegistro1111(ARegistro1001.Registro1101);
    end;
  end
  else
    raise Exception.Create('Registro 1101 é obrigatório!');
end;

procedure TBloco_1.WriteRegistro1110(const ARegistro1100: TRegistro1100);
var
  i: Integer;
begin
  if (ARegistro1100.Registro1110List.Count > 0) then
  begin
    for i := 0 to ARegistro1100.Registro1110List.Count -1 do
      with ARegistro1100.Registro1110List.Items[i] do
      begin
        Add(
          LFill(REG)+
          LFill(DT_DOC)+
          LFill(ADRCSTIndicadorResponsavelRetencaoToString( COD_RESP_RET))+
          LFill(CST_CSOSN)+
          LFill(CHAVE,44)+
          LFill(N_NF,0)+
          LFill(CNPJ_EMIT,14)+
          LFill(UF_EMIT,2)+
          LFill(CNPJ_DEST,14)+
          LFill(UF_DEST,2)+
          LFill(CFOP,4)+
          LFill(N_ITEM,0)+
          LFill(UNID_ITEM)+
          LFill(QTD_ENTRADA,9,3)+
          LFill(VL_UNIT_ITEM,9,2)+
          LFill(VL_BC_ICMS_ST,9,2)+
          LFill(VL_ICMS_SUPORT_ENTR,9,2),
          False
          );
      end;
  end
  else
    raise Exception.Create('Necessário ter pelo menos um 1110');
end;

procedure TBloco_1.WriteRegistro1111(const ARegistro1101: TRegistro1101);
var
  i: Integer;
begin
  if (ARegistro1101.Registro1111List.Count > 0) then
  begin
    for i := 0 to ARegistro1101.Registro1111List.Count -1 do
      with ARegistro1101.Registro1111List.Items[i] do
      begin
        Add(
          LFill(REG)+
          LFill(DT_DOC)+
          LFill(ADRCSTIndicadorResponsavelRetencaoToString( COD_RESP_RET))+
          LFill(CST_CSOSN)+
          LFill(CHAVE,44)+
          LFill(N_NF,0)+
          LFill(CNPJ_EMIT,14)+
          LFill(UF_EMIT,2)+
          LFill(CNPJ_DEST,14)+
          LFill(UF_DEST,2)+
          LFill(CFOP,4)+
          LFill(N_ITEM,0)+
          LFill(UNID_ITEM)+
          LFill(QTD_ENTRADA,9,3)+
          LFill(VL_UNIT_ITEM,9,2)+
          LFill(VL_BC_ICMS_ST,9,2)+
          LFill(VL_ICMS_SUPORT_ENTR,9,2),
          False
          );
      end;
  end
  else
    raise Exception.Create('Necessário ter pelo menos um 1111');
end;

procedure TBloco_1.WriteRegistro1120(const ARegistro1100: TRegistro1100);
var
  i: Integer;
begin
  if (ARegistro1100.Registro1120List.Count > 0) then
  begin
    for i := 0 to ARegistro1100.Registro1120List.Count -1 do
      with ARegistro1100.Registro1120List.Items[i] do
      begin
        Add(
          LFill(REG)+
          LFill(DT_DOC)+
          LFill(CST_CSOSN)+
          LFill(CHAVE,44)+
          LFill(N_NF,0)+
          LFill(CNPJ_EMIT,14)+
          LFill(UF_EMIT,2)+
          LFill(CNPJ_DEST,14)+
          LFill(UF_DEST,2)+
          LFill(CFOP,4)+
          LFill(N_ITEM,0)+
          LFill(UNID_ITEM)+
          LFill(QTD_DEVOLVIDA,9,3)+
          LFill(VL_UNIT_ITEM,9,2)+
          LFill(VL_BC_ICMS_ST,9,2)+
          LFill(VL_ICMS_SUPORT_ENTR,9,2)+
          LFill(CHAVE_REF,44)+
          LFill(N_ITEM_REF,0),
          False
          );
      end;
  end

end;

procedure TBloco_1.WriteRegistro1200(const ARegistro1000: TRegistro1000);
begin
  if Assigned(ARegistro1000.Registro1200) then
  begin
    with ARegistro1000.Registro1200 do
    begin
      Add(
        LFill(REG) +
        LFill(QTD_TOT_SAIDA,9,3)+
        LFill(VL_TOT_ICMS_EFETIVO,9,2)+
        LFill(VL_CONFRONTO_ICMS_ENTRADA,9,2)+
        LFill(RESULT_RECUPERAR_RESSARCIR,9,2)+
        LFill(RESULT_COMPLEMENTAR,9,2)+
        LFill(APUR_ICMSST_RECUPERAR_RESSARCIR,9,2)+
        LFill(APUR_ICMSST_COMPLEMENTAR,9,2)+
        LFill(APUR_FECOP_RESSARCIR,9,2)+
        LFill(APUR_FECOP_COMPLEMENTAR,9,2),
        False
        );
      WriteRegistro1210(ARegistro1000.Registro1200);
      WriteRegistro1220(ARegistro1000.Registro1200);
    end;
  end
end;

procedure TBloco_1.WriteRegistro1210(const ARegistro1200: TRegistro1200);
var
  i: Integer;
begin
  if (ARegistro1200.Registro1210List.Count > 0) then
  begin
    for i := 0 to ARegistro1200.Registro1210List.Count -1 do
      with ARegistro1200.Registro1210List.Items[i] do
      begin
        Add(
          LFill(REG)+
          LFill(DT_DOC) +
          LFill(CST_CSOSN) +
          LFill(CHAVE,44) +
          LFill(N_NF,0) +
          LFill(CNPJ_EMIT,14) +
          LFill(UF_EMIT,2) +
          LFill(CNPJ_CPF_DEST) +
          LFill(UF_DEST,2) +
          LFill(CFOP,4) +
          LFill(N_ITEM,0) +
          LFill(UNID_ITEM)+
          LFill(QTD_SAIDA,9,3) +
          LFill(VL_UNIT_ITEM,9,2) +
          LFill(VL_ICMS_EFETIVO,9,2),
          False
          );
      end;
  end
  else
    raise Exception.Create('Necessário ter pelo menos um 1210');

end;

procedure TBloco_1.WriteRegistro1220(const ARegistro1200: TRegistro1200);
var
  i: Integer;
begin
  if (ARegistro1200.Registro1220List.Count > 0) then
  begin
    for i := 0 to ARegistro1200.Registro1220List.Count -1 do
      with ARegistro1200.Registro1220List.Items[i] do
      begin
        Add(
          LFill(REG)+
          LFill(DT_DOC) +
          LFill(CST_CSOSN) +
          LFill(CHAVE,44) +
          LFill(N_NF,0) +
          LFill(CNPJ_EMIT,14) +
          LFill(UF_EMIT,2) +
          LFill(CNPJ_CPF_DEST)+
          LFill(UF_DEST,2)+
          LFill(CFOP,4)+
          LFill(N_ITEM,0) +
          LFill(UNID_ITEM) +
          LFill(QTD_DEVOLVIDA, 9,3) +
          LFill(VL_UNIT_ITEM,9,2) +
          LFill(VL_ICMS_EFETIVO,9,2) +
          LFill(CHAVE_REF,44)+
          LFill(N_ITEM_REF,0),
          False
          );
      end;
  end
end;

procedure TBloco_1.WriteRegistro1300(const ARegistro1000: TRegistro1000);
begin
  if Assigned(ARegistro1000.Registro1300) then
  begin
    with ARegistro1000.Registro1300 do
    begin
      Add(
        LFill(REG) +
        LFill(QTD_TOT_SAIDA,9,3)+
        LFill(VL_TOT_ICMS_EFETIVO,9,2)+
        LFill(VL_CONFRONTO_ICMS_ENTRADA,9,2) +
        LFill(RESULT_RECUPERAR_RESSARCIR,9,2)+
        LFill(APUR_ICMSST_RECUPERAR_RESSARCIR,9,2)+
        LFill(APUR_FECOP_RESSARCIR,9,2),
        False
        );
      WriteRegistro1310(ARegistro1000.Registro1300);
      WriteRegistro1320(ARegistro1000.Registro1300);
    end;
  end
end;

procedure TBloco_1.WriteRegistro1310(const ARegistro1300: TRegistro1300);
var
  i: Integer;
begin
  if (ARegistro1300.Registro1310List.Count > 0) then
  begin
    for i := 0 to ARegistro1300.Registro1310List.Count -1 do
      with ARegistro1300.Registro1310List.Items[i] do
      begin
        Add(
          LFill(REG)+
          LFill(DT_DOC)+
          LFill(CST_CSOSN) +
          LFill(CHAVE,44) +
          LFill(N_NF,0) +
          LFill(CNPJ_EMIT,14) +
          LFill(UF_EMIT,2) +
          LFill(CNPJ_CPF_DEST) +
          LFill(UF_DEST,2) +
          LFill(CFOP,4) +
          LFill(N_ITEM,0) +
          LFill(UNID_ITEM)+
          LFill(QTD_SAIDA,9,3)+
          LFill(VL_UNIT_ITEM,9,2)+
          LFill(VL_ICMS_EFETIVO,9,2),
          False
          );
      end;
  end
  else
    raise Exception.Create('Necessário ter pelo menos um 1310');
end;

procedure TBloco_1.WriteRegistro1320(const ARegistro1300: TRegistro1300);
var
  i: Integer;
begin
  if (ARegistro1300.Registro1320List.Count > 0) then
  begin
    for i := 0 to ARegistro1300.Registro1320List.Count -1 do
      with ARegistro1300.Registro1320List.Items[i] do
      begin
        Add(
          LFill(REG) +
          LFill(DT_DOC) +
          LFill(CST_CSOSN) +
          LFill(CHAVE,44) +
          LFill(N_NF,0) +
          LFill(CNPJ_EMIT,14) +
          LFill(UF_EMIT,2) +
          LFill(CNPJ_CPF_DEST) +
          LFill(UF_DEST,2) +
          LFill(CFOP,4) +
          LFill(N_ITEM,0) +
          LFill(UNID_ITEM) +
          LFill(QTD_DEVOLVIDA,9,3) +
          LFill(VL_UNIT_ITEM,9,2) +
          LFill(VL_ICMS_EFETIVO,9,2) +
          LFill(CHAVE_REF,44) +
          LFill(N_ITEM_REF,0),
          False
          );
      end;
  end
end;

procedure TBloco_1.WriteRegistro1400(const ARegistro1000: TRegistro1000);
begin
  if Assigned(ARegistro1000.Registro1400) then
  begin
    with ARegistro1000.Registro1400 do
    begin
      Add(
        LFill(REG) +
        LFill(QTD_TOT_SAIDA,9,3)+
        LFill(VL_TOT_ICMS_EFETIVO,9,2)+
        LFill(VL_CONFRONTO_ICMS_ENTRADA,9,2) +
        LFill(APUR_ICMSST_RECUPERAR_RESSARCIR,9,2),
        False
        );
      WriteRegistro1410(ARegistro1000.Registro1400);
      WriteRegistro1420(ARegistro1000.Registro1400);
    end;
  end
end;

procedure TBloco_1.WriteRegistro1410(const ARegistro1400: TRegistro1400);
var
  i: Integer;
begin
  if (ARegistro1400.Registro1410List.Count > 0) then
  begin
    for i := 0 to ARegistro1400.Registro1410List.Count -1 do
      with ARegistro1400.Registro1410List.Items[i] do
      begin
        Add(
          LFill(REG)+
          LFill(DT_DOC)+
          LFill(CST_CSOSN) +
          LFill(CHAVE,44) +
          LFill(N_NF,0) +
          LFill(CNPJ_EMIT,14) +
          LFill(UF_EMIT,2) +
          LFill(CNPJ_CPF_DEST) +
          LFill(UF_DEST,2) +
          LFill(CFOP,4) +
          LFill(N_ITEM,0) +
          LFill(UNID_ITEM)+
          LFill(QTD_SAIDA,9,3)+
          LFill(VL_UNIT_ITEM,9,2)+
          LFill(VL_ICMS_EFETIVO,9,2),
          False
          );
      end;
  end
  else
    raise Exception.Create('Necessário ter pelo menos um 1410');
end;

procedure TBloco_1.WriteRegistro1420(const ARegistro1400: TRegistro1400);
var
  i: Integer;
begin
  if (ARegistro1400.Registro1420List.Count > 0) then
  begin
    for i := 0 to ARegistro1400.Registro1420List.Count -1 do
      with ARegistro1400.Registro1420List.Items[i] do
      begin
        Add(
          LFill(REG) +
          LFill(DT_DOC) +
          LFill(CST_CSOSN) +
          LFill(CHAVE,44) +
          LFill(N_NF,0) +
          LFill(CNPJ_EMIT,14) +
          LFill(UF_EMIT,2) +
          LFill(CNPJ_CPF_DEST) +
          LFill(UF_DEST,2) +
          LFill(CFOP,4) +
          LFill(N_ITEM,0) +
          LFill(UNID_ITEM) +
          LFill(QTD_DEVOLVIDA,9,3) +
          LFill(VL_UNIT_ITEM,9,2) +
          LFill(VL_ICMS_EFETIVO,9,2) +
          LFill(CHAVE_REF,44) +
          LFill(N_ITEM_REF,0),
          False
          );
      end;
  end
end;

procedure TBloco_1.WriteRegistro1500(const ARegistro1000: TRegistro1000);
begin
  if Assigned(ARegistro1000.Registro1500) then
  begin
    with ARegistro1000.Registro1500 do
    begin
      Add(
        LFill(REG) +
        LFill(QTD_TOT_SAIDA,9,3)+
        LFill(VL_ICMSST_UNIT_ENTR,9,4)+
        LFill(APUR_ICMSST_RECUPERAR_RESSARCIR,9,2) +
        LFill(MVA_ICMSST,9,2),
        False
        );
      WriteRegistro1510(ARegistro1000.Registro1500);
      WriteRegistro1520(ARegistro1000.Registro1500);
    end;
  end
end;

procedure TBloco_1.WriteRegistro1510(const ARegistro1500: TRegistro1500);
var
  i: Integer;
begin
  if (ARegistro1500.Registro1510List.Count > 0) then
  begin
    for i := 0 to ARegistro1500.Registro1510List.Count -1 do
      with ARegistro1500.Registro1510List.Items[i] do
      begin
        Add(
          LFill(REG)+
          LFill(DT_DOC) +
          LFill(CST_CSOSN) +
          LFill(CHAVE,44) +
          LFill(N_NF,0) +
          LFill(CNPJ_EMIT,14) +
          LFill(UF_EMIT,2) +
          LFill(CNPJ_CPF_DEST) +
          LFill(UF_DEST,2) +
          LFill(CFOP,4) +
          LFill(N_ITEM,0) +
          LFill(UNID_ITEM) +
          LFill(QTD_SAIDA,9,3) +
          LFill(VL_UNIT_ITEM,9,2),
          False
          );
      end;
  end
  else
    raise Exception.Create('Necessário ter pelo menos um 1510');

end;

procedure TBloco_1.WriteRegistro1520(const ARegistro1500: TRegistro1500);
var
  i: Integer;
begin
  if (ARegistro1500.Registro1520List.Count > 0) then
  begin
    for i := 0 to ARegistro1500.Registro1520List.Count -1 do
      with ARegistro1500.Registro1520List.Items[i] do
      begin
        Add(
          LFill(REG) +
          LFill(DT_DOC) +
          LFill(CST_CSOSN) +
          LFill(CHAVE,44) +
          LFill(N_NF,0) +
          LFill(CNPJ_EMIT,14) +
          LFill(UF_EMIT,2) +
          LFill(CNPJ_CPF_DEST) +
          LFill(UF_DEST,2) +
          LFill(CFOP,4) +
          LFill(N_ITEM,0) +
          LFill(UNID_ITEM) +
          LFill(QTD_DEVOLVIDA,9,3)+
          LFill(VL_UNIT_ITEM,9,2)+
          LFill(CHAVE_REF,44) +
          LFill(N_ITEM_REF,0),
          False
          );
      end;
  end
end;

procedure TBloco_1.WriteRegistro1999(const ARegistro1999: TRegistro1999);
begin
   with ARegistro1999 do
   begin
     Add(LFill(REG) +
         LFill(QTD_LIN,4),
         False);
   end;
end;

constructor TBloco_1.Create;
begin
  inherited;
  FRegistro1000List := TRegistro1000List.Create;
  FRegistro1001List := TRegistro1001List.Create;
end;

destructor TBloco_1.Destroy;
begin
  FRegistro1000List.Destroy;
  FRegistro1001List.Destroy;
  inherited Destroy;
end;

function TBloco_1.GetLayout: TADRCSTLayout;
begin
  Result := FLayout;
end;

function TBloco_1.Registro1000New: TRegistro1000;
begin
  Result := TRegistro1000.Create(FRegistro1000List.Registro1999);
  FRegistro1000List.Add(Result);
end;

function TBloco_1.Registro1001New: TRegistro1001;
begin
  Result := TRegistro1001.Create(FRegistro1001List.Registro1999);
  FRegistro1001List.Add(Result);
end;

procedure TBloco_1.SetLayout(const Value: TADRCSTLayout);
begin
  FLayout := Value;
  if Layout = lyADRCST then
    FRegistro1999   := FRegistro1000List.Registro1999
  else
    FRegistro1999   := FRegistro1001List.Registro1999;
end;

procedure TBloco_1.WriteRegistro1000;
var
  i : integer;
begin
  if Assigned(FRegistro1000List) then
  begin
    for i := 0 to FRegistro1000List.Count -1 do
    begin
      with FRegistro1000List.Items[i] do
      begin
        Add(LFill(REG) +
            LFill(ADRCSTIndicadorProdutoFECOPToString( IND_FECOP)) +
            LFill(COD_ITEM) +
            LFill(COD_BARRAS) +
            LFill(COD_ANP) +
            LFill(NCM) +
            LFill(CEST) +
            LFill(DESCR_ITEM) +
            LFill(UNID_ITEM) +
            LFill(ALIQ_ICMS_ITEM,2,2) +
            LFill(ALIQ_FECOP,1,2) +
            LFill(QTD_TOT_ENTRADA,9,3) +
            LFill(QTD_TOT_SAIDA,9,3),
            False
          );

        WriteRegistro1010(FRegistro1000List.Items[i]);
        WriteRegistro1100(FRegistro1000List.Items[i]);


        WriteRegistro1200(FRegistro1000List.Items[i]);

        WriteRegistro1300(FRegistro1000List.Items[i]);

        WriteRegistro1400(FRegistro1000List.Items[i]);

        WriteRegistro1500(FRegistro1000List.Items[i]);
      end;
    end;
    WriteRegistro1999( FRegistro1000List.Registro1999);
  end;
end;

procedure TBloco_1.WriteRegistro1001;
var
  i : integer;
begin
  if Assigned(FRegistro1001List) then
  begin
    for i := 0 to FRegistro1001List.Count -1 do
    begin
      with FRegistro1001List.Items[i] do
      begin
        Add(LFill(REG) +
            LFill(ADRCSTIndicadorProdutoFECOPToString( IND_FECOP)) +
            LFill(COD_ITEM) +
            LFill(COD_BARRAS) +
            LFill(COD_ANP) +
            LFill(NCM) +
            LFill(CEST) +
            LFill(DESCR_ITEM) +
            LFill(UNID_ITEM) +
            LFill(ALIQ_ICMS_ITEM,2,2) +
            LFill(ALIQ_FECOP,1,2),
            False
          );

        WriteRegistro1101(FRegistro1001List.Items[i]);
      end;
    end;
  end;
end;

end.
