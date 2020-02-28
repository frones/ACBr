{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Juliana Tamizou, Isaque Pinheiro e              }
{							   Nilson Sergio								   }	  
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

unit ACBrLFDBloco_A_Class;

interface

uses SysUtils, Classes, DateUtils, ACBrLFD3505, ACBrLFDBlocos, ACBrLFDBloco_A,
     ACBrTXTClass;


type

  /// Bloco A - DOCUMENTOS FISCAIS DO ISS

  { TBloco_A }

  TBloco_A = class(TACBrLFD3505)
  private
    FRegistroA001: TRegistroA001;
    FRegistroA990: TRegistroA990;

    FRegistroA020Count: Integer;
    FRegistroA025Count: Integer;
    FRegistroA035Count: Integer;
    FRegistroA045Count: Integer;
    FRegistroA040Count: Integer;
    FRegistroA050Count: Integer;
    FRegistroA055Count: Integer;
    FRegistroA200Count: Integer;
    FRegistroA300Count: Integer;
    FRegistroA310Count: Integer;
    FRegistroA320Count: Integer;
    FRegistroA330Count: Integer;
    FRegistroA350Count: Integer;
    FRegistroA355Count: Integer;
    FRegistroA360Count: Integer;
    FRegistroA365Count: Integer;
    FRegistroA370Count: Integer;
    FRegistroA380Count: Integer;

    procedure WriteRegistroA020(RegA001: TRegistroA001);
    procedure WriteRegistroA025(RegA020: TRegistroA020);
    procedure WriteRegistroA035(RegA020: TRegistroA020);
    procedure WriteRegistroA040(RegA020: TRegistroA020);
    procedure WriteRegistroA045(RegA040: TRegistroA040);
    procedure WriteRegistroA050(RegA020: TRegistroA020);
    procedure WriteRegistroA200(RegA020: TRegistroA020);
    procedure WriteRegistroA300(RegA001: TRegistroA001);
    procedure WriteRegistroA310(RegA300: TRegistroA300);
    procedure WriteRegistroA320(RegA001: TRegistroA001);
    procedure WriteRegistroA330(RegA300: TRegistroA300);
    procedure WriteRegistroA350(RegA001: TRegistroA001);
    procedure WriteRegistroA355(RegA350: TRegistroA350);
    procedure WriteRegistroA360(RegA350: TRegistroA350);
    procedure WriteRegistroA365(RegA360: TRegistroA360);
    procedure WriteRegistroA370(RegA001: TRegistroA001);
    procedure WriteRegistroA380(RegA370: TRegistroA370);

    procedure CriaRegistros;
    procedure LiberaRegistros;
  public
    constructor Create;           /// Create
    destructor Destroy; override; /// Destroy
    procedure LimpaRegistros;

    function RegistroA001New: TRegistroA001;
    function RegistroA020New: TRegistroA020;
    function RegistroA025New: TRegistroA025;
    function RegistroA035New: TRegistroA035;
    function RegistroA040New: TRegistroA040;
    function RegistroA050New: TRegistroA050;
    function RegistroA055New: TRegistroA055;
    function RegistroA200New: TRegistroA200;
    function RegistroA300New: TRegistroA300;
    function RegistroA310New: TRegistroA310;
    function RegistroA320New: TRegistroA320;
    function RegistroA330New: TRegistroA330;
    function RegistroA350New: TRegistroA350;
    function RegistroA355New: TRegistroA355;
    function RegistroA360New: TRegistroA360;
    function RegistroA365New: TRegistroA365;
    function RegistroA370New: TRegistroA370;
    function RegistroA380New: TRegistroA380;

    procedure WriteRegistroA001;
    procedure WriteRegistroA990;

    property RegistroA001: TRegistroA001 read FRegistroA001 write FRegistroA001;
    property RegistroA990: TRegistroA990 read FRegistroA990 write FRegistroA990;

    property RegistroA020Count: Integer read FRegistroA020Count write FRegistroA020Count;
    property RegistroA025Count: Integer read FRegistroA025Count write FRegistroA025Count;
    property RegistroA035Count: Integer read FRegistroA035Count write FRegistroA035Count;
    property RegistroA040Count: Integer read FRegistroA040Count write FRegistroA040Count;
    property RegistroA045Count: Integer read FRegistroA045Count write FRegistroA045Count;
    property RegistroA050Count: Integer read FRegistroA050Count write FRegistroA050Count;
    property RegistroA055Count: Integer read FRegistroA055Count write FRegistroA055Count;
    property RegistroA200Count: Integer read FRegistroA200Count write FRegistroA200Count;
    property RegistroA300Count: Integer read FRegistroA300Count write FRegistroA300Count;
    property RegistroA310Count: Integer read FRegistroA310Count write FRegistroA310Count;
    property RegistroA320Count: Integer read FRegistroA320Count write FRegistroA320Count;
    property RegistroA330Count: Integer read FRegistroA330Count write FRegistroA330Count;
    property RegistroA350Count: Integer read FRegistroA350Count write FRegistroA350Count;
    property RegistroA355Count: Integer read FRegistroA355Count write FRegistroA355Count;
    property RegistroA360Count: Integer read FRegistroA360Count write FRegistroA360Count;
    property RegistroA365Count: Integer read FRegistroA365Count write FRegistroA365Count;
    property RegistroA370Count: Integer read FRegistroA370Count write FRegistroA370Count;
    property RegistroA380Count: Integer read FRegistroA380Count write FRegistroA380Count;
  end;

implementation

uses StrUtils;

{ TBloco_A }

constructor TBloco_A.Create ;
begin
  inherited ;
  CriaRegistros;
end;

destructor TBloco_A.Destroy;
begin
  LiberaRegistros;
  inherited;
end;

procedure TBloco_A.CriaRegistros;
begin
  FRegistroA001 := TRegistroA001.Create;
  FRegistroA990 := TRegistroA990.Create;

  FRegistroA020Count := 0;
  FRegistroA025Count := 0;
  FRegistroA035Count := 0;
  FRegistroA040Count := 0;
  FRegistroA045Count := 0;
  FRegistroA050Count := 0;
  FRegistroA055Count := 0;
  FRegistroA200Count := 0;
  FRegistroA300Count := 0;
  FRegistroA310Count := 0;
  FRegistroA320Count := 0;
  FRegistroA330Count := 0;
  FRegistroA350Count := 0;
  FRegistroA355Count := 0;
  FRegistroA360Count := 0;
  FRegistroA365Count := 0;
  FRegistroA370Count := 0;
  FRegistroA380Count := 0;

  FRegistroA990.QTD_LIN_A := 0;
end;

procedure TBloco_A.LiberaRegistros;
begin
  FRegistroA001.Free;
  FRegistroA990.Free;
end;

procedure TBloco_A.LimpaRegistros;
begin
  /// Limpa os Registros
  LiberaRegistros;
  Conteudo.Clear;

  /// Recriar os Registros Limpos
  CriaRegistros;
end;

function TBloco_A.RegistroA001New: TRegistroA001;
begin
  Result := FRegistroA001;
end;

function TBloco_A.RegistroA020New: TRegistroA020;
begin
  Result := FRegistroA001.RegistroA020.New(FRegistroA001);
end;

function TBloco_A.RegistroA025New: TRegistroA025;
var
  A020: TRegistroA020;
begin
  with FRegistroA001.RegistroA020 do
    A020 := Items[ AchaUltimoPai('A020', 'A025') ];
  Result := A020.RegistroA025.New(A020);
end;

function TBloco_A.RegistroA035New: TRegistroA035;
var
  A020: TRegistroA020;
begin
  with FRegistroA001.RegistroA020 do
    A020 := Items[ AchaUltimoPai('A020', 'A035') ];
  Result := A020.RegistroA035;
end;

function TBloco_A.RegistroA040New: TRegistroA040;
var
  A020: TRegistroA020;
begin
  with FRegistroA001.RegistroA020 do
    A020 := Items[ AchaUltimoPai('A020', 'A040') ];
  Result := A020.RegistroA040;
end;

function TBloco_A.RegistroA050New: TRegistroA050;
var
  A020: TRegistroA020;
begin
  with FRegistroA001.RegistroA020 do
    A020 := Items[ AchaUltimoPai('A020', 'A050') ];
  Result := A020.RegistroA050.New(A020);
end;

function TBloco_A.RegistroA055New: TRegistroA055;
var
  A020: TRegistroA020;
  A050: TRegistroA050;
begin
  with FRegistroA001.RegistroA020 do
    A020 := Items[ AchaUltimoPai('A020', 'A050') ];
  with A020.RegistroA050 do
    A050 := Items[ AchaUltimoPai('A050', 'A055') ];
  Result := A050.RegistroA055.New(A050);
end;

function TBloco_A.RegistroA200New: TRegistroA200;
var
  A020: TRegistroA020;
begin
  with FRegistroA001.RegistroA020 do
    A020 := Items[ AchaUltimoPai('A020', 'A200') ];
  Result := A020.RegistroA200.New(A020);
end;

function TBloco_A.RegistroA300New: TRegistroA300;
begin
  Result := FRegistroA001.RegistroA300.New(FRegistroA001);
end;

function TBloco_A.RegistroA310New: TRegistroA310;
var
  A300: TRegistroA300;
begin
  with FRegistroA001.RegistroA300 do
    A300 := Items[ AchaUltimoPai('A300', 'A310') ];
  Result := A300.RegistroA310.New(A300);
end;

function TBloco_A.RegistroA320New: TRegistroA320;
begin
  Result := FRegistroA001.RegistroA320.New(FRegistroA001);
end;

function TBloco_A.RegistroA330New: TRegistroA330;
var
  A320: TRegistroA320;
begin
  with FRegistroA001.RegistroA320 do
    A320 := Items[ AchaUltimoPai('A320', 'A330') ];
  Result := A320.RegistroA330.New(A320);
end;

function TBloco_A.RegistroA350New: TRegistroA350;
begin
  Result := FRegistroA001.RegistroA350.New(FRegistroA001);
end;

function TBloco_A.RegistroA355New: TRegistroA355;
var
  A350: TRegistroA350;
begin
  with FRegistroA001.RegistroA350 do
    A350 := Items[ AchaUltimoPai('A350', 'A355') ];
  Result := A350.RegistroA355;
end;

function TBloco_A.RegistroA360New: TRegistroA360;
var
  A350: TRegistroA350;
begin
  with FRegistroA001.RegistroA350 do
    A350 := Items[ AchaUltimoPai('A350', 'A360') ];
  Result := A350.RegistroA360.New(A350);
end;

function TBloco_A.RegistroA365New: TRegistroA365;
var
  A350: TRegistroA350;
  A360: TRegistroA360;
begin
  with FRegistroA001.RegistroA350 do
    A350 := Items[ AchaUltimoPai('A350', 'A360') ];
  with A350.RegistroA360 do
    A360 := Items[ AchaUltimoPai('A360', 'A365') ];
  Result := A360.RegistroA365;
end;

function TBloco_A.RegistroA370New: TRegistroA370;
begin
  Result := FRegistroA001.RegistroA370.New(FRegistroA001);
end;

function TBloco_A.RegistroA380New: TRegistroA380;
var
  A370: TRegistroA370;
begin
  with FRegistroA001.RegistroA370 do
    A370 := Items[ AchaUltimoPai('A370', 'A380') ];
  Result := A370.RegistroA380.New(A370);
end;

procedure TBloco_A.WriteRegistroA001;
begin
    if Assigned(FRegistroA001) then
    begin
       with FRegistroA001 do
       begin
          Add( LFill( 'A001' ) +
               LFill( Integer(IND_MOV), 0 ) +
               LFill(COD_MUN,7) ) ;

          if IND_MOV = imlComDados then
          begin
            WriteRegistroA020(FRegistroA001) ;
            WriteRegistroA300(FRegistroA001) ;
            WriteRegistroA350(FRegistroA001);
          end;
       end;

       RegistroA990.QTD_LIN_A := RegistroA990.QTD_LIN_A + 1;
    end;
end;

procedure TBloco_A.WriteRegistroA020(RegA001: TRegistroA001);
var
  intFor: Integer;
begin
   if Assigned(RegA001.RegistroA020) then
   begin
      for intFor := 0 to RegA001.RegistroA020.Count - 1 do
      begin
         with RegA001.RegistroA020.Items[intFor] do
         begin
            Add( LFill('A020') +
                 LFill(Integer(IND_OPER),1) +
                 LFill(Integer(IND_EMIT),1) +
                 LFill(COD_PART) +
                 LFill(COD_MOD)  +
                 LFill(Integer(COD_SIT),2) +
                 LFill(SER) +
                 LFill(SUB) +
                 LFill(NUM_DOC) +
                 LFill(DT_DOC) +
                 LFill(CFPS) +
                 LFill(COD_MUN_SERV,7) +
                 LFill(COD_NAT) +
                 LFill(VL_DOC,0,2, true ) +
                 LFill(integer(IND_PGTO),1 )+
                 LFill(VL_SUB) +
                 LFill(VL_DESC,0,2, true ) +
                 LFill(VL_SERV,0,2, true ) +
                 LFill(VL_MAT_PROP,0,2, true ) +
                 LFill(VL_MAT_TERC,0,2, true ) +
                 LFill(VL_DA,0,2, true ) +
                 LFill(VL_BC_ISS,0,2, true ) +
                 LFill(VL_ISS,0,2, true ) +
                 LFill(VL_BC_RT_ISS,0,2, true ) +
                 LFill(VL_RT_ISS,0,2, true ) +
                 LFill(COD_INF_OBS) );

         end;
         RegistroA990.QTD_LIN_A := RegistroA990.QTD_LIN_A + 1;

         WriteRegistroA025(RegA001.RegistroA020.Items[intFor]);

         WriteRegistroA200(RegA001.RegistroA020.Items[intFor]);
      end;
      FRegistroA020Count := FRegistroA020Count + RegA001.RegistroA020.Count;
  end;
end;

procedure TBloco_A.WriteRegistroA025(RegA020: TRegistroA020);
var
  intFor: Integer;
begin
  if Assigned(RegA020.RegistroA025) then
  begin
    for intFor := 0 to RegA020.RegistroA025.Count - 1 do
    begin
       with RegA020.RegistroA025.Items[intFor] do
       begin
         Add( LFill('A025') +
              DFill(VL_BC_IRRF,2) +
              DFill(ALIQ_IRRF,2) +
              DFill(VL_IRRF,2) +
              DFill(ALIQ_PIS,2) +
              DFill(VL_PIS,2) +
              DFill(ALIQ_COFINS,2) +
              DFill(VL_COFINS,2) +
              DFill(VL_BC_PREV,2) +
              DFill(VL_PREV,2) );
       end;
       RegistroA990.QTD_LIN_A := RegistroA990.QTD_LIN_A + 1;
    end;
    FRegistroA025Count := FRegistroA025Count + RegA020.RegistroA025.Count;
  end;
end;

procedure TBloco_A.WriteRegistroA035(RegA020: TRegistroA020);
begin

end;

procedure TBloco_A.WriteRegistroA040(RegA020: TRegistroA020);
begin

end;

procedure TBloco_A.WriteRegistroA045(RegA040: TRegistroA040);
begin

end;

procedure TBloco_A.WriteRegistroA050(RegA020: TRegistroA020);
begin

end;

procedure TBloco_A.WriteRegistroA200(RegA020: TRegistroA020);
var
  intFor: Integer;
begin
  if Assigned(RegA020.RegistroA200) then
  begin
    for intFor := 0 to RegA020.RegistroA200.Count - 1 do
      begin
         with RegA020.RegistroA200.Items[intFor] do
         begin
            Add( LFill('A200') +
                 LFill(Integer(NUM_ITEM),2) +
                 LFill(COD_ITEM) +
                 DFill(VL_UNIT,3)  +
                 DFill(QTD,3) +
                 LFill(UNID) +
                 LFill(VL_ITEM,2) +
                 LFill(VL_DESC_I,2) +
                 LFill(CTISS) +
                 LFill(VL_BC_ISS_I,2) +
                 LFill(ALIQ_ISS,2) +
                 LFill(VL_ISS_I,2) );
         end;
         RegistroA990.QTD_LIN_A := RegistroA990.QTD_LIN_A + 1;
      end;
      FRegistroA200Count := FRegistroA200Count + RegA020.RegistroA200.Count;
  end;
End;

procedure TBloco_A.WriteRegistroA300(RegA001: TRegistroA001);
var
  intFor: Integer;
begin
   if Assigned(RegA001.RegistroA300) then
   begin
      for intFor := 0 to RegA001.RegistroA300.Count - 1 do
      begin
         with RegA001.RegistroA300.Items[intFor] do
         begin
            Add( LFill('A300') +
                 LFill(CPF_CONS) +
                 LFill(CNPJ_CONS) +
                 LFill(COD_MOD)  +
                 LFill(Integer(COD_SIT),2) +
                 LFill(SER) +
                 LFill(SUB) +
                 LFill(NUM_DOC) +
                 LFill(DT_DOC) +
                 LFill(COP,4) +
                 LFill(VL_DOC,2) +
                 LFill(VL_DESC,2) +
                 LFill(VL_SERV,2) +
                 LFill(VL_MAT_PROP,2) +
                 LFill(VL_DA,2) +
                 LFill(VL_BC_ISS,2) +
                 LFill(VL_ISS,2) +
                 LFill(COD_INF_OBS) );
         end;
         RegistroA990.QTD_LIN_A := RegistroA990.QTD_LIN_A + 1;
      end;
      FRegistroA300Count := FRegistroA300Count + RegA001.RegistroA300.Count;
  end;
end;

procedure TBloco_A.WriteRegistroA310(RegA300: TRegistroA300);
begin

end;

procedure TBloco_A.WriteRegistroA320(RegA001: TRegistroA001);
begin

end;

procedure TBloco_A.WriteRegistroA330(RegA300: TRegistroA300);
begin

end;

procedure TBloco_A.WriteRegistroA350(RegA001: TRegistroA001);
var
  intFor: Integer;
  A350: TRegistroA350;
begin
   if Assigned(RegA001.RegistroA350) then
   begin
      for intFor := 0 to RegA001.RegistroA350.Count - 1 do
      begin
         A350 := RegA001.RegistroA350.Items[intFor];
         with A350 do
         begin
            Add( LFill('A350') +
                 LFill(CPF_CONS) +
                 LFill(CNPJ_CONS) +
                 LFill(COD_MOD)  +
                 LFill(Integer(COD_SIT),2) +
                 LFill(ECF_CX,0) +
                 LFill(ECF_FAB) +
                 LFill(CRO,0) +
                 LFill(CRZ,0) +
                 LFill(NUM_DOC) +
                 LFill(DT_DOC) +
                 LFill(COP,4) +
                 LFill(VL_DOC,2) +
                 LFill(VL_CANC_ISS,2) +
                 LFill(VL_CANC_ICMS,2) +
                 LFill(VL_CANC_ISS + VL_CANC_ICMS,2) +
                 LFill(VL_DESC_ISS,2) +
                 LFill(VL_DESC_ICMS,2) +
                 LFill(VL_DESC_ISS + VL_DESC_ICMS,2) +
                 LFill(VL_ACMO_ISS,2) +
                 LFill(VL_ACMO_ICMS,2) +
                 LFill(VL_ACMO_ISS + VL_ACMO_ICMS,2) +
                 LFill(VL_BC_ISS,2) +
                 LFill(VL_ISS,2) +
                 LFill(VL_ISN_ISS,2) +
                 LFill(VL_NT_ISS,2) +
                 LFill(VL_RET_ISS,2) );
         end;
         WriteRegistroA360(A350);
         RegistroA990.QTD_LIN_A := RegistroA990.QTD_LIN_A + 1;
      end;

      FRegistroA350Count := FRegistroA350Count + RegA001.RegistroA350.Count;
  end;
end;

procedure TBloco_A.WriteRegistroA355(RegA350: TRegistroA350);
begin

end;

procedure TBloco_A.WriteRegistroA360(RegA350: TRegistroA350);
var
  intFor: Integer;
  A360: TRegistroA360;
begin
   for intFor := 0 to RegA350.RegistroA360.Count - 1 do
   begin
      A360 := RegA350.RegistroA360.Items[intFor];
      with A360 do
      begin
         Add( LFill('A360') +
              LFill(NUM_ITEM,0) +
              LFill(COD_ITEM) +
              DFill(VL_UNIT, 3) +
              DFill(QTD,3) +
              LFill(QTDCANC,0) +
              LFill(UNID) +
              LFill(VL_ITEM, 2) +
              LFill(VL_DESC_I, 2) +
              LFill(VL_CANC_I, 2) +
              LFill(VL_ACMO_I, 2) +
              LFill(CTISS) +
              LFill(VL_BC_ISS_I, 2) +
              LFill(ALIQ_ISS, 2) +
              LFill(VL_ISS_I, 2) +
              LFill(VL_ISN_ISS_I, 2) +
              LFill(VL_NT_ISS_I, 2) +
              LFill(VL_RT_ISS_I, 2) );
    end;

    FRegistroA990.QTD_LIN_A := FRegistroA990.QTD_LIN_A + 1;
  end;

  FRegistroA360Count := FRegistroA360Count + RegA350.RegistroA360.Count;
end;

procedure TBloco_A.WriteRegistroA365(RegA360: TRegistroA360);
begin

end;

procedure TBloco_A.WriteRegistroA370(RegA001: TRegistroA001);
begin

end;

procedure TBloco_A.WriteRegistroA380(RegA370: TRegistroA370);
begin

end;

procedure TBloco_A.WriteRegistroA990;
var
  strLinha: String;
begin
//--Before
  strLinha := '';

  if Assigned(RegistroA990) then
  begin
     with RegistroA990 do
     begin
       QTD_LIN_A := QTD_LIN_A + 1;
       ///
       strLinha := LFill('A990') +
                   LFill(QTD_LIN_A,0);
       Add(strLinha);
     end;
  end;
end;

end.
