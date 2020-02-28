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

unit ACBrLFDBloco_B_Class;

interface

uses SysUtils, Classes, DateUtils, ACBrLFD3505, ACBrLFDBlocos, ACBrLFDBloco_B,
     ACBrTXTClass;


type
  { TBloco_B }

  TBloco_B = class(TACBrLFD3505)
  private
    FRegistroB001: TRegistroB001;
    FRegistroB990: TRegistroB990;

    FRegistroB020Count: Integer;
    FRegistroB025Count: Integer;
    FRegistroB030Count: Integer;
    FRegistroB035Count: Integer;
    FRegistroB040Count: Integer;
    FRegistroB045Count: Integer;
    FRegistroB050Count: Integer;
    FRegistroB055Count: Integer;
    FRegistroB350Count: Integer;
    FRegistroB400Count: Integer;
    FRegistroB410Count: Integer;
    FRegistroB420Count: Integer;
    FRegistroB430Count: Integer;
    FRegistroB440Count: Integer;
    FRegistroB450Count: Integer;
    FRegistroB460Count: Integer;
    FRegistroB465Count: Integer;
    FRegistroB470Count: Integer;
    FRegistroB475Count: Integer;
    FRegistroB480Count: Integer;
    FRegistroB490Count: Integer;
    FRegistroB500Count: Integer;
    FRegistroB510Count: Integer;
    FRegistroB600Count: Integer;
    FRegistroB700Count: Integer;

    procedure WriteRegistroB020(RegB001: TRegistroB001);
    procedure WriteRegistroB025(RegB020: TRegistroB020);
    procedure WriteRegistroB030(RegB001: TRegistroB001);
    procedure WriteRegistroB035(RegB030: TRegistroB030);
    procedure WriteRegistroB040(RegB001: TRegistroB001);
    procedure WriteRegistroB045(RegB040: TRegistroB040);
    procedure WriteRegistroB050(RegB001: TRegistroB001);
    procedure WriteRegistroB055(RegB050: TRegistroB050);
    procedure WriteRegistroB350(RegB001: TRegistroB001);
    procedure WriteRegistroB400(RegB001: TRegistroB001);
    procedure WriteRegistroB410(RegB400: TRegistroB400);
    procedure WriteRegistroB420(RegB400: TRegistroB400);
    procedure WriteRegistroB430(RegB400: TRegistroB400);
    procedure WriteRegistroB440(RegB400: TRegistroB400);
    procedure WriteRegistroB450(RegB400: TRegistroB400);
    procedure WriteRegistroB460(RegB400: TRegistroB400);
    procedure WriteRegistroB465(RegB460: TRegistroB460);
    procedure WriteRegistroB470(RegB400: TRegistroB400);
    procedure WriteRegistroB475(RegB475: TRegistroB475);
    procedure WriteRegistroB480(RegB475: TRegistroB475);
    procedure WriteRegistroB490(RegB400: TRegistroB400);
    procedure WriteRegistroB500(RegB400: TRegistroB400);
    procedure WriteRegistroB510(RegB500: TRegistroB500);
    procedure WriteRegistroB600(RegB400: TRegistroB400);
    procedure WriteRegistroB700(RegB001: TRegistroB001);

    procedure CriaRegistros;
    procedure LiberaRegistros;
  public
    constructor Create;           /// Create
    destructor Destroy; override; /// Destroy
    procedure LimpaRegistros;

    function RegistroB001New: TRegistroB001;
    function RegistroB020New: TRegistroB020;
    function RegistroB025New: TRegistroB025;
    function RegistroB030New: TRegistroB030;
    function RegistroB035New: TRegistroB035;
    function RegistroB040New: TRegistroB040;
    function RegistroB045New: TRegistroB045;
    function RegistroB050New: TRegistroB050;
    function RegistroB055New: TRegistroB055;
    function RegistroB350New: TRegistroB350;
    function RegistroB400New: TRegistroB400;
    function RegistroB410New: TRegistroB410;
    function RegistroB420New: TRegistroB420;
    function RegistroB430New: TRegistroB430;
    function RegistroB440New: TRegistroB440;
    function RegistroB450New: TRegistroB450;
    function RegistroB460New: TRegistroB460;
    function RegistroB465New: TRegistroB465;
    function RegistroB470New: TRegistroB470;
    function RegistroB475New: TRegistroB475;
    function RegistroB480New: TRegistroB480;
    function RegistroB490New: TRegistroB490;
    function RegistroB500New: TRegistroB500;
    function RegistroB510New: TRegistroB510;
    function RegistroB600New: TRegistroB600;
    function RegistroB700New: TRegistroB700;

    procedure WriteRegistroB001;
    procedure WriteRegistroB990;

    property RegistroB001: TRegistroB001 read FRegistroB001 write FRegistroB001;
    property RegistroB990: TRegistroB990 read FRegistroB990 write FRegistroB990;

    property RegistroB020Count: Integer read FRegistroB020Count write FRegistroB020Count;
    property RegistroB025Count: Integer read FRegistroB025Count write FRegistroB025Count;
    property RegistroB030Count: Integer read FRegistroB030Count write FRegistroB030Count;
    property RegistroB035Count: Integer read FRegistroB035Count write FRegistroB035Count;
    property RegistroB040Count: Integer read FRegistroB040Count write FRegistroB040Count;
    property RegistroB045Count: Integer read FRegistroB045Count write FRegistroB045Count;
    property RegistroB050Count: Integer read FRegistroB050Count write FRegistroB050Count;
    property RegistroB055Count: Integer read FRegistroB055Count write FRegistroB055Count;
    property RegistroB350Count: Integer read FRegistroB350Count write FRegistroB350Count;
    property RegistroB400Count: Integer read FRegistroB400Count write FRegistroB400Count;
    property RegistroB410Count: Integer read FRegistroB410Count write FRegistroB410Count;
    property RegistroB420Count: Integer read FRegistroB420Count write FRegistroB420Count;
    property RegistroB430Count: Integer read FRegistroB430Count write FRegistroB430Count;
    property RegistroB440Count: Integer read FRegistroB440Count write FRegistroB440Count;
    property RegistroB450Count: Integer read FRegistroB450Count write FRegistroB450Count;
    property RegistroB460Count: Integer read FRegistroB460Count write FRegistroB460Count;
    property RegistroB465Count: Integer read FRegistroB465Count write FRegistroB465Count;
    property RegistroB470Count: Integer read FRegistroB470Count write FRegistroB470Count;
    property RegistroB475Count: Integer read FRegistroB475Count write FRegistroB475Count;
    property RegistroB480Count: Integer read FRegistroB480Count write FRegistroB480Count;
    property RegistroB490Count: Integer read FRegistroB490Count write FRegistroB490Count;
    property RegistroB500Count: Integer read FRegistroB500Count write FRegistroB500Count;
    property RegistroB510Count: Integer read FRegistroB510Count write FRegistroB510Count;
    property RegistroB600Count: Integer read FRegistroB600Count write FRegistroB600Count;
    property RegistroB700Count: Integer read FRegistroB700Count write FRegistroB700Count;
  end;

implementation

uses StrUtils;

{ TBloco_B }

constructor TBloco_B.Create ;
begin
  inherited ;
  CriaRegistros;
end;

destructor TBloco_B.Destroy;
begin
  LiberaRegistros;
  inherited;
end;

procedure TBloco_B.CriaRegistros;
begin
  FRegistroB001 := TRegistroB001.Create;
  FRegistroB990 := TRegistroB990.Create;

  FRegistroB020Count := 0;
  FRegistroB025Count := 0;
  FRegistroB030Count := 0;
  FRegistroB035Count := 0;
  FRegistroB040Count := 0;
  FRegistroB045Count := 0;
  FRegistroB050Count := 0;
  FRegistroB055Count := 0;
  FRegistroB350Count := 0;
  FRegistroB400Count := 0;
  FRegistroB410Count := 0;
  FRegistroB420Count := 0;
  FRegistroB430Count := 0;
  FRegistroB440Count := 0;
  FRegistroB450Count := 0;
  FRegistroB460Count := 0;
  FRegistroB465Count := 0;
  FRegistroB470Count := 0;
  FRegistroB475Count := 0;
  FRegistroB480Count := 0;
  FRegistroB490Count := 0;
  FRegistroB500Count := 0;
  FRegistroB510Count := 0;
  FRegistroB600Count := 0;
  FRegistroB700Count := 0;

  FRegistroB990.QTD_LIN_B := 0;
end;

procedure TBloco_B.LiberaRegistros;
begin
  FRegistroB001.Free;
  FRegistroB990.Free;
end;

procedure TBloco_B.LimpaRegistros;
begin
  /// Limpa os Registros
  LiberaRegistros;
  Conteudo.Clear;

  /// Recriar os Registros Limpos
  CriaRegistros;
end;

function TBloco_B.RegistroB001New: TRegistroB001;
begin
  Result := FRegistroB001;
end;

function TBloco_B.RegistroB020New: TRegistroB020;
begin
  Result := FRegistroB001.RegistroB020.New(FRegistroB001);
end;

function TBloco_B.RegistroB025New: TRegistroB025;
var
  B020: TRegistroB020;
begin
  with FRegistroB001.RegistroB020 do
    B020 := Items[ AchaUltimoPai('B020', 'B025') ];
  Result := B020.RegistroB025.New(B020);
end;

function TBloco_B.RegistroB030New: TRegistroB030;
begin
  Result := FRegistroB001.RegistroB030.New(FRegistroB001);
end;

function TBloco_B.RegistroB035New: TRegistroB035;
var
  B030: TRegistroB030;
begin
  with FRegistroB001.RegistroB030 do
    B030 := Items[ AchaUltimoPai('B030', 'B035') ];
  Result := B030.RegistroB035.New(B030);
end;

function TBloco_B.RegistroB040New: TRegistroB040;
begin
  Result := FRegistroB001.RegistroB040.New(FRegistroB001);
end;

function TBloco_B.RegistroB045New: TRegistroB045;
var
  B040: TRegistroB040;
begin
  with FRegistroB001.RegistroB040 do
    B040 := Items[ AchaUltimoPai('B040', 'B045') ];
  Result := B040.RegistroB045.New(B040);
end;

function TBloco_B.RegistroB050New: TRegistroB050;
begin
  Result := FRegistroB001.RegistroB050.New(FRegistroB001);
end;

function TBloco_B.RegistroB055New: TRegistroB055;
var
  B050: TRegistroB050;
begin
  with FRegistroB001.RegistroB050 do
    B050 := Items[ AchaUltimoPai('B050', 'B055') ];
  Result := B050.RegistroB055.New(B050);
end;

function TBloco_B.RegistroB350New: TRegistroB350;
begin
  Result := FRegistroB001.RegistroB350.New(FRegistroB001);
end;

function TBloco_B.RegistroB400New: TRegistroB400;
begin
  Result := FRegistroB001.RegistroB400.New(FRegistroB001);
end;

function TBloco_B.RegistroB410New: TRegistroB410;
var
  B400: TRegistroB400;
begin
  with FRegistroB001.RegistroB400 do
    B400 := Items[ AchaUltimoPai('B400', 'B410') ];
  Result := B400.RegistroB410.New(B400);
end;

function TBloco_B.RegistroB420New: TRegistroB420;
var
  B400: TRegistroB400;
begin
  with FRegistroB001.RegistroB400 do
    B400 := Items[ AchaUltimoPai('B400', 'B420') ];
  Result := B400.RegistroB420.New(B400);
end;

function TBloco_B.RegistroB430New: TRegistroB430;
var
  B400: TRegistroB400;
begin
  with FRegistroB001.RegistroB400 do
    B400 := Items[ AchaUltimoPai('B400', 'B430') ];
  Result := B400.RegistroB430.New(B400);
end;

function TBloco_B.RegistroB440New: TRegistroB440;
var
  B400: TRegistroB400;
begin
  with FRegistroB001.RegistroB400 do
    B400 := Items[ AchaUltimoPai('B400', 'B440') ];
  Result := B400.RegistroB440.New(B400);
end;

function TBloco_B.RegistroB450New: TRegistroB450;
var
  B400: TRegistroB400;
begin
  with FRegistroB001.RegistroB400 do
    B400 := Items[ AchaUltimoPai('B400', 'B450') ];
  Result := B400.RegistroB450.New(B400);
end;

function TBloco_B.RegistroB460New: TRegistroB460;
var
  B400: TRegistroB400;
begin
  with FRegistroB001.RegistroB400 do
    B400 := Items[ AchaUltimoPai('B400', 'B460') ];
  Result := B400.RegistroB460.New(B400);
end;

function TBloco_B.RegistroB465New: TRegistroB465;
var
  B400: TRegistroB400;
  B460: TRegistroB460;
begin
  with FRegistroB001.RegistroB400 do
    B400 := Items[ AchaUltimoPai('B400', 'B410') ];
  with B400.RegistroB460 do
    B460 := Items[ AchaUltimoPai('B460', 'B465') ];
  Result := B460.RegistroB465.New(B460);
end;

function TBloco_B.RegistroB470New: TRegistroB470;
var
  B400: TRegistroB400;
begin
  with FRegistroB001.RegistroB400 do
    B400 := Items[ AchaUltimoPai('B400', 'B470') ];
  Result := B400.RegistroB470.New(B400);
end;

function TBloco_B.RegistroB475New: TRegistroB475;
var
  B400: TRegistroB400;
  B470: TRegistroB470;
begin
  with FRegistroB001.RegistroB400 do
    B400 := Items[ AchaUltimoPai('B400', 'B410') ];
  with B400.RegistroB470 do
    B470 := Items[ AchaUltimoPai('B470', 'B475') ];
  Result := B470.RegistroB475.New(B470);
end;

function TBloco_B.RegistroB480New: TRegistroB480;
var
  B400: TRegistroB400;
  B470: TRegistroB470;
begin
  with FRegistroB001.RegistroB400 do
    B400 := Items[ AchaUltimoPai('B400', 'B410') ];
  with B400.RegistroB470 do
    B470 := Items[ AchaUltimoPai('B470', 'B480') ];
  Result := B470.RegistroB480.New(B470);
end;

function TBloco_B.RegistroB490New: TRegistroB490;
var
  B400: TRegistroB400;
begin
  with FRegistroB001.RegistroB400 do
    B400 := Items[ AchaUltimoPai('B400', 'B490') ];
  Result := B400.RegistroB490.New(B400);
end;

function TBloco_B.RegistroB500New: TRegistroB500;
var
  B400: TRegistroB400;
begin
  with FRegistroB001.RegistroB400 do
    B400 := Items[ AchaUltimoPai('B400', 'B500') ];
  Result := B400.RegistroB500.New(B400);
end;

function TBloco_B.RegistroB510New: TRegistroB510;
var
  B400: TRegistroB400;
  B500: TRegistroB500;
begin
  with FRegistroB001.RegistroB400 do
    B400 := Items[ AchaUltimoPai('B400', 'B410') ];
  with B400.RegistroB500 do
    B500 := Items[ AchaUltimoPai('B500', 'B510') ];
  Result := B500.RegistroB510.New(B500);
end;

function TBloco_B.RegistroB600New: TRegistroB600;
var
  B400: TRegistroB400;
begin
  with FRegistroB001.RegistroB400 do
    B400 := Items[ AchaUltimoPai('B400', 'B600') ];
  Result := B400.RegistroB600.New(B400);
end;

function TBloco_B.RegistroB700New: TRegistroB700;
begin
  Result := FRegistroB001.RegistroB700.New(FRegistroB001);
end;

procedure TBloco_B.WriteRegistroB001;
begin
  if Assigned(FRegistroB001) then
    begin
       with FRegistroB001 do
       begin
          Add( LFill( 'B001' ) +
               LFill( Integer(IND_MOV), 0 ) +
               LFill(COD_MUN,7)) ;

          if IND_MOV = imlComDados then
          begin
            WriteRegistroB020(FRegistroB001);
            WriteRegistroB400(FRegistroB001);
          end;
       end;

       RegistroB990.QTD_LIN_B := RegistroB990.QTD_LIN_B + 1;
    end;
end;

procedure TBloco_B.WriteRegistroB020(RegB001: TRegistroB001);
var
  intFor: Integer;
begin
  if Assigned(RegB001.RegistroB020) then
  begin
    for intFor := 0 to RegB001.RegistroB020.Count - 1 do
    begin
      with RegB001.RegistroB020.Items[intFor] do
      begin
        Add( LFill('B020') +
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
             LFill(NUM_LCTO) +
             LFill(integer(COD_MUN_SERV),7) +
             DFill(VL_CONT,2) +
             DFill(VL_MAT_TERC,2) +
             DFill(VL_SUB,2) +
             DFill(VL_ISNT_ISS,2) +
             DFill(VL_DED_BC,2) +
             DFill(VL_BC_ISS,2) +
             DFill(VL_BC_ISS_RT,2) +
             DFill(VL_ISS_RT,2) +
             DFill(VL_ISS,2) +
             LFill('') );
      end;
      WriteRegistroB025(RegB001.RegistroB020.Items[intFor]);

      RegistroB990.QTD_LIN_B := RegistroB990.QTD_LIN_B + 1;
    end;
    FRegistroB020Count := FRegistroB020Count + RegB001.RegistroB020.Count;
  end;
end;

procedure TBloco_B.WriteRegistroB025(RegB020: TRegistroB020);
var
  intFor: Integer;
begin
  if Assigned(RegB020.RegistroB025) then
  begin
    for intFor := 0 to RegB020.RegistroB025.Count - 1 do
    begin
       with RegB020.RegistroB025.Items[intFor] do
       begin
         Add( LFill('B025') +
              DFill(VL_CONT_P,2) +
              DFill(VL_BC_ISS_P,2) +
              DFill(ALIQ_ISS,2) +
              DFill(VL_ISS_P,2) +
              DFill(VL_ISNT_ISS_P,2) );
       end;
       RegistroB990.QTD_LIN_B := RegistroB990.QTD_LIN_B + 1;
    end;
    FRegistroB025Count := FRegistroB025Count + RegB020.RegistroB025.Count;
  end;
end;

procedure TBloco_B.WriteRegistroB030(RegB001: TRegistroB001);
begin

end;

procedure TBloco_B.WriteRegistroB035(RegB030: TRegistroB030);
begin

end;

procedure TBloco_B.WriteRegistroB040(RegB001: TRegistroB001);
begin

end;

procedure TBloco_B.WriteRegistroB045(RegB040: TRegistroB040);
begin

end;

procedure TBloco_B.WriteRegistroB050(RegB001: TRegistroB001);
begin

end;

procedure TBloco_B.WriteRegistroB055(RegB050: TRegistroB050);
begin

end;

procedure TBloco_B.WriteRegistroB350(RegB001: TRegistroB001);
begin

end;

procedure TBloco_B.WriteRegistroB400(RegB001: TRegistroB001);
var
  intFor: Integer;
begin
  if Assigned(RegB001.RegistroB400) then
  begin
    for intFor := 0 to RegB001.RegistroB400.Count - 1 do
    begin
      with RegB001.RegistroB400.Items[intFor] do
      begin
        Add(LFill('B400') +
            LFill(DT_INI) +
            LFill(DT_FIN) +
            LFill(Integer(IND_DAD),1));

        if IND_DAD = imlComDados then
        begin
          WriteRegistroB410(RegB001.RegistroB400.Items[intFor]);
          WriteRegistroB420(RegB001.RegistroB400.Items[intFor]);
          WriteRegistroB430(RegB001.RegistroB400.Items[intFor]);
          WriteRegistroB440(RegB001.RegistroB400.Items[intFor]);
          WriteRegistroB450(RegB001.RegistroB400.Items[intFor]);
          WriteRegistroB460(RegB001.RegistroB400.Items[intFor]);
          WriteRegistroB470(RegB001.RegistroB400.Items[intFor]);
          WriteRegistroB490(RegB001.RegistroB400.Items[intFor]);
        end;
      end;
      RegistroB990.QTD_LIN_B := RegistroB990.QTD_LIN_B + 1;
    end;
    FRegistroB400Count := FRegistroB400Count + RegB001.RegistroB400.Count;
  end;
end;

procedure TBloco_B.WriteRegistroB410(RegB400: TRegistroB400);
var
  intFor: Integer;
begin
  if Assigned(RegB400.RegistroB410) then
  begin
    for intFor := 0 to RegB400.RegistroB410.Count - 1 do
    begin
       with RegB400.RegistroB410.Items[intFor] do
       begin
         Add( LFill('B410') +
              LFill(Integer(IND_TOT),1) +
              DFill(VL_CONT,2) +
              DFill(VL_BC_ISS,2) +
              DFill(VL_DED_BC,2) +
              DFill(VL_RT,2) +
              DFill(VL_ISNT_ISS,2) +
              DFill(VL_ISS,2));
       end;
       RegistroB990.QTD_LIN_B := RegistroB990.QTD_LIN_B + 1;
    end;
    FRegistroB410Count := FRegistroB410Count + RegB400.RegistroB410.Count;
  end;
end;

procedure TBloco_B.WriteRegistroB420(RegB400: TRegistroB400);
var
  intFor: Integer;
begin
  if Assigned(RegB400.RegistroB420) then
  begin
    for intFor := 0 to RegB400.RegistroB420.Count - 1 do
    begin
       with RegB400.RegistroB420.Items[intFor] do
       begin
         Add( LFill('B420') +
              DFill(VL_CONT,2) +
              DFill(VL_BC_ISS,2) +
              DFill(ALIQ_ISS,2) +
              DFill(VL_BC_ISS_RT,2) +
              DFill(VL_RT,2) +
              DFill(VL_ISNT_ISS,2) +
              DFill(VL_ISS,2));
       end;
       RegistroB990.QTD_LIN_B := RegistroB990.QTD_LIN_B + 1;
    end;
    FRegistroB420Count := FRegistroB420Count + RegB400.RegistroB420.Count;
  end;
end;

procedure TBloco_B.WriteRegistroB430(RegB400: TRegistroB400);
var
  intFor: Integer;
begin
  if Assigned(RegB400.RegistroB430) then
  begin
    for intFor := 0 to RegB400.RegistroB430.Count - 1 do
    begin
       with RegB400.RegistroB430.Items[intFor] do
       begin
         Add( LFill('B430') +
              LFill(CFPS) +
              DFill(VL_CONT,2) +
              DFill(VL_BC_ISS,2) +
              DFill(VL_MAT_TERC,2) +
              DFill(VL_SUB,2) +
              DFill(VL_ISNT_ISS,2) +
              DFill(VL_DED_BC,2) +
              DFill(VL_BC_ISS_RT,2) +
              DFill(VL_ISS_RT,2) +
              DFill(VL_ISS,2));
       end;
       RegistroB990.QTD_LIN_B := RegistroB990.QTD_LIN_B + 1;
    end;
    FRegistroB430Count := FRegistroB430Count + RegB400.RegistroB430.Count;
  end;
end;

procedure TBloco_B.WriteRegistroB440(RegB400: TRegistroB400);
var
  intFor: Integer;
begin
  if Assigned(RegB400.RegistroB440) then
  begin
    for intFor := 0 to RegB400.RegistroB440.Count - 1 do
    begin
       with RegB400.RegistroB440.Items[intFor] do
       begin
         Add( LFill('B440') +
              LFill(Integer(IND_OPER), 0) +
              LFill(COD_PART) +
              DFill(VL_CONT_RT,2) +
              DFill(VL_BC_ISS_RT,2) +
              DFill(ALIQ_ISS,2) +
              DFill(VL_ISS_RT,2));
       end;
       RegistroB990.QTD_LIN_B := RegistroB990.QTD_LIN_B + 1;
    end;
    FRegistroB440Count := FRegistroB440Count + RegB400.RegistroB440.Count;
  end;
end;

procedure TBloco_B.WriteRegistroB450(RegB400: TRegistroB400);
var
  intFor: Integer;
begin
  if Assigned(RegB400.RegistroB450) then
  begin
    for intFor := 0 to RegB400.RegistroB450.Count - 1 do
    begin
       with RegB400.RegistroB450.Items[intFor] do
       begin
         Add( LFill('B450') +
              LFill(Integer(IND_OPER), 0) +
              LFill(COD_MUN_SERV,7) +
              DFill(VL_CONT,2) +
              DFill(VL_BC_ISS,2) +
              DFill(VL_ISNT_ISS,2) +
              DFill(VL_DED_BC,2) +
              DFill(VL_ISS_RT,2) +
              DFill(VL_ISS,2));
       end;
       RegistroB990.QTD_LIN_B := RegistroB990.QTD_LIN_B + 1;
    end;
    FRegistroB450Count := FRegistroB450Count + RegB400.RegistroB450.Count;
  end;
end;

procedure TBloco_B.WriteRegistroB460(RegB400: TRegistroB400);
var
  intFor: Integer;
begin
  if Assigned(RegB400.RegistroB460) then
  begin
    for intFor := 0 to RegB400.RegistroB460.Count - 1 do
    begin
      with RegB400.RegistroB460.Items[intFor] do
      begin

        Add( LFill('B460') +
             LFill(Integer(IND_DED)) +
             DFill(VL_DED,2) +
             LFill(NUM_PROC) +
             LFill(Integer(IND_PROC)) +
             LFill(PROC) +
             LFill(COD_INF_OBS) );
      end;
      WriteRegistroB465(RegB400.RegistroB460.Items[intFor]);

      RegistroB990.QTD_LIN_B := RegistroB990.QTD_LIN_B + 1;
    end;
    FRegistroB460Count := FRegistroB460Count + RegB400.RegistroB460.Count;
  end;
end;

procedure TBloco_B.WriteRegistroB465(RegB460: TRegistroB460);
var
  intFor: Integer;
begin
  if Assigned(RegB460.RegistroB465) then
  begin
    for intFor := 0 to RegB460.RegistroB465.Count - 1 do
    begin
       with RegB460.RegistroB465.Items[intFor] do
       begin
         Add( LFill('B465') +
              LFill(Integer(IND_COMP)) +
              DFill(VL_CRED,2) +
              DFill(VL_COMP,2) +
              DFill(PER_FISCAL,2) +
              DFill(VL_RES,2) +
              LFill(COMENT) );
       end;
       RegistroB990.QTD_LIN_B := RegistroB990.QTD_LIN_B + 1;
    end;
    FRegistroB465Count := FRegistroB465Count + RegB460.RegistroB465.Count;
  end;
end;

procedure TBloco_B.WriteRegistroB470(RegB400: TRegistroB400);
var
  intFor: Integer;
begin
  if Assigned(RegB400.RegistroB470) then
  begin
    for intFor := 0 to RegB400.RegistroB470.Count - 1 do
    begin
       with RegB400.RegistroB470.Items[intFor] do
       begin
         Add( LFill('B470') +
              DFill(VL_CONT,2) +
              DFill(VL_MAT_TERC,2) +
              DFill(VL_MAT_PROP,2) +
              DFill(VL_SUB,2) +
              DFill(VL_ISNT,2) +
              DFill(VL_DED_BC,2) +
              DFill(VL_BC_ISS,2) +
              DFill(VL_BC_ISS_RT,2) +
              DFill(VL_ISS,2) +
              DFill(VL_ISS_RT,2) +
              DFill(VL_DED,2) +
              DFill(VL_ISS_REC,2) +
              DFill(VL_ISS_ST,2) +
              DFill(VL_ISS_FIL,2) +
              DFill(VL_ISS_RT_REC,2));
       end;
       RegistroB990.QTD_LIN_B := RegistroB990.QTD_LIN_B + 1;
    end;
    FRegistroB470Count := FRegistroB470Count + RegB400.RegistroB470.Count;
  end;
end;

procedure TBloco_B.WriteRegistroB475(RegB475: TRegistroB475);
begin

end;

procedure TBloco_B.WriteRegistroB480(RegB475: TRegistroB475);
begin

end;

procedure TBloco_B.WriteRegistroB490(RegB400: TRegistroB400);
var
  intFor: Integer;
begin
  if Assigned(RegB400.RegistroB490) then
  begin
    for intFor := 0 to RegB400.RegistroB490.Count - 1 do
    begin
       with RegB400.RegistroB490.Items[intFor] do
       begin
         Add( LFill('B490') +
              LFill(COD_OR, 2) +
              DFill(VL_OR, 2) +
              LFill(DT_VCTO) +
              LFill(COD_REC) +
              LFill(COD_MUN_SERV, 7) +
              LFill(NUM_PROC) +
              LFill(Integer(IND_PROC), 0) +
              LFill(PROC) +
              LFill(COD_INF_OBS));
       end;
       RegistroB990.QTD_LIN_B := RegistroB990.QTD_LIN_B + 1;
    end;
    FRegistroB490Count := FRegistroB490Count + RegB400.RegistroB490.Count;
  end;
end;

procedure TBloco_B.WriteRegistroB500(RegB400: TRegistroB400);
begin

end;

procedure TBloco_B.WriteRegistroB510(RegB500: TRegistroB500);
begin

end;

procedure TBloco_B.WriteRegistroB600(RegB400: TRegistroB400);
begin

end;

procedure TBloco_B.WriteRegistroB700(RegB001: TRegistroB001);
begin

end;

procedure TBloco_B.WriteRegistroB990;
var
  strLinha: String;
begin
//--Before
  strLinha := '';

  if Assigned(RegistroB990) then
  begin
     with RegistroB990 do
     begin
       QTD_LIN_B := QTD_LIN_B + 1;
       ///
       strLinha := LFill('B990') +
                   LFill(QTD_LIN_B,0);
       Add(strLinha);
     end;
  end;
end;

end.
