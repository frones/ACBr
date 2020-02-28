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

unit ACBrLFDBloco_C_Class;

interface

uses SysUtils, Classes, DateUtils, ACBrLFD3505, ACBrLFDBlocos, ACBrLFDBloco_C,
     ACBrTXTClass;


type
  /// TBLOCO_C - Documentos Fiscais do ICMS e do IPI - Mercadorias

  { TBloco_C }

  TBloco_C = class(TACBrLFD3505)
  private
    FRegistroC001: TRegistroC001;
    FRegistroC990: TRegistroC990;

    FRegistroC020Count: Integer;
    FRegistroC030Count: Integer;
    FRegistroC035Count: Integer;
    FRegistroC040Count: Integer;
    FRegistroC250Count: Integer;
    FRegistroC255Count: Integer;
    FRegistroC260Count: Integer;
    FRegistroC265Count: Integer;
    FRegistroC300Count: Integer;
    FRegistroC305Count: Integer;
    FRegistroC310Count: Integer;
    FRegistroC315Count: Integer;
    FRegistroC320Count: Integer;
    FRegistroC325Count: Integer;
    FRegistroC500Count: Integer;
    FRegistroC550Count: Integer;
    FRegistroC555Count: Integer;
    FRegistroC560Count: Integer;
    FRegistroC570Count: Integer;
    FRegistroC575Count: Integer;
    FRegistroC600Count: Integer;
    FRegistroC605Count: Integer;
    FRegistroC610Count: Integer;
    FRegistroC615Count: Integer;
    FRegistroC620Count: Integer;
    FRegistroC625Count: Integer;
    FRegistroC700Count: Integer;
    FRegistroC705Count: Integer;
    FRegistroC710Count: Integer;
    FRegistroC715Count: Integer;
    FRegistroC720Count: Integer;
    FRegistroC750Count: Integer;
    FRegistroC755Count: Integer;
    FRegistroC770Count: Integer;
    FRegistroC775Count: Integer;

    procedure WriteRegistroC020(RegC001: TRegistroC001);
    procedure WriteRegistroC030(RegC020: TRegistroC020);
    procedure WriteRegistroC035(RegC030: TRegistroC030);
    procedure WriteRegistroC040(RegC020: TRegistroC020);
    procedure WriteRegistroC050(RegC020: TRegistroC020);
    procedure WriteRegistroC060(RegC020: TRegistroC020);
    procedure WriteRegistroC200(RegC020: TRegistroC020);
    procedure WriteRegistroC250(RegC020: TRegistroC020);
    procedure WriteRegistroC255(RegC250: TRegistroC250);
    procedure WriteRegistroC260(RegC250: TRegistroC250);
    procedure WriteRegistroC300(RegC020: TRegistroC020);
    procedure WriteRegistroC305(RegC300: TRegistroC300);
    procedure WriteRegistroC310(RegC300: TRegistroC300);
    procedure WriteRegistroC315(RegC300: TRegistroC300);
    procedure WriteRegistroC320(RegC300: TRegistroC300);
    procedure WriteRegistroC325(RegC300: TRegistroC300);
    procedure WriteRegistroC500(RegC020: TRegistroC020);
    procedure WriteRegistroC550(RegC001: TRegistroC001);
    procedure WriteRegistroC555(RegC550: TRegistroC550);
    procedure WriteRegistroC560(RegC550: TRegistroC550);
    procedure WriteRegistroC570(RegC001: TRegistroC001);
    procedure WriteRegistroC575(RegC570: TRegistroC570);
    procedure WriteRegistroC580(RegC001: TRegistroC001);
    procedure WriteRegistroC600(RegC001: TRegistroC001);
    procedure WriteRegistroC605(RegC600: TRegistroC600);
    procedure WriteRegistroC610(RegC600: TRegistroC600);
    procedure WriteRegistroC615(RegC610: TRegistroC610);
    procedure WriteRegistroC620(RegC001: TRegistroC001);
    procedure WriteRegistroC625(RegC620: TRegistroC620);
    procedure WriteRegistroC640(RegC001: TRegistroC001);
    procedure WriteRegistroC700(RegC001: TRegistroC001);
    procedure WriteRegistroC705(RegC700: TRegistroC700);
    procedure WriteRegistroC750(RegC700: TRegistroC700);
    procedure WriteRegistroC760(RegC700: TRegistroC700);
    procedure WriteRegistroC770(RegC001: TRegistroC001);
    procedure WriteRegistroC775(RegC770: TRegistroC770);
    procedure WriteRegistroC780(RegC770: TRegistroC770);

    procedure CriaRegistros;
    procedure LiberaRegistros;

    function SituacaoDoctoToStr(ASituacao: TACBrlSituacaoDocto): String;
  public
    constructor Create;           /// Create
    destructor Destroy; override; /// Destroy
    procedure LimpaRegistros;

    function RegistroC001New: TRegistroC001;
    function RegistroC020New: TRegistroC020;
    function RegistroC030New: TRegistroC030;
    function RegistroC035New: TRegistroC035;
    function RegistroC040New: TRegistroC040;
    function RegistroC250New: TRegistroC250;
    function RegistroC255New: TRegistroC255;
    function RegistroC260New: TRegistroC260;
    function RegistroC265New: TRegistroC265;
    function RegistroC300New: TRegistroC300;
    function RegistroC305New: TRegistroC305;
    function RegistroC310New: TRegistroC310;
    function RegistroC315New: TRegistroC315;
    function RegistroC320New: TRegistroC320;
    function RegistroC325New: TRegistroC325;
    function RegistroC550New: TRegistroC550;
    function RegistroC555New: TRegistroC555;
    function RegistroC560New: TRegistroC560;
    function RegistroC570New: TRegistroC570;
    function RegistroC575New: TRegistroC575;
    function RegistroC600New: TRegistroC600;
    function RegistroC605New: TRegistroC605;
    function RegistroC610New: TRegistroC610;
    function RegistroC615New: TRegistroC615;
    function RegistroC620New: TRegistroC620;
    function RegistroC625New: TRegistroC625;
    function RegistroC700New: TRegistroC700;
    function RegistroC705New: TRegistroC705;
    function RegistroC710New: TRegistroC710;
    function RegistroC715New: TRegistroC715;
    function RegistroC720New: TRegistroC720;
    function RegistroC750New: TRegistroC750;
    function RegistroC755New: TRegistroC755;
    function RegistroC770New: TRegistroC770;
    function RegistroC775New: TRegistroC775;

    procedure WriteRegistroC001;
    procedure WriteRegistroC990;

    property RegistroC001: TRegistroC001 read FRegistroC001 write FRegistroC001;
    property RegistroC990: TRegistroC990 read FRegistroC990 write FRegistroC990;

    property RegistroC020Count: Integer read FRegistroC020Count write FRegistroC020Count;
    property RegistroC030Count: Integer read FRegistroC030Count write FRegistroC030Count;
    property RegistroC035Count: Integer read FRegistroC035Count write FRegistroC035Count;
    property RegistroC040Count: Integer read FRegistroC040Count write FRegistroC040Count;
    property RegistroC250Count: Integer read FRegistroC250Count write FRegistroC250Count;
    property RegistroC255Count: Integer read FRegistroC255Count write FRegistroC255Count;
    property RegistroC260Count: Integer read FRegistroC260Count write FRegistroC260Count;
    property RegistroC265Count: Integer read FRegistroC265Count write FRegistroC265Count;
    property RegistroC300Count: Integer read FRegistroC300Count write FRegistroC300Count;
    property RegistroC305Count: Integer read FRegistroC305Count write FRegistroC305Count;
    property RegistroC310Count: Integer read FRegistroC310Count write FRegistroC310Count;
    property RegistroC315Count: Integer read FRegistroC315Count write FRegistroC315Count;
    property RegistroC320Count: Integer read FRegistroC320Count write FRegistroC320Count;
    property RegistroC325Count: Integer read FRegistroC325Count write FRegistroC325Count;
    property RegistroC500Count: Integer read FRegistroC500Count write FRegistroC500Count;
    property RegistroC550Count: Integer read FRegistroC550Count write FRegistroC550Count;
    property RegistroC555Count: Integer read FRegistroC555Count write FRegistroC555Count;
    property RegistroC560Count: Integer read FRegistroC560Count write FRegistroC560Count;
    property RegistroC570Count: Integer read FRegistroC570Count write FRegistroC570Count;
    property RegistroC575Count: Integer read FRegistroC575Count write FRegistroC575Count;
    property RegistroC600Count: Integer read FRegistroC600Count write FRegistroC600Count;
    property RegistroC605Count: Integer read FRegistroC605Count write FRegistroC605Count;
    property RegistroC610Count: Integer read FRegistroC610Count write FRegistroC610Count;
    property RegistroC615Count: Integer read FRegistroC615Count write FRegistroC615Count;
    property RegistroC620Count: Integer read FRegistroC620Count write FRegistroC620Count;
    property RegistroC625Count: Integer read FRegistroC625Count write FRegistroC625Count;
    property RegistroC700Count: Integer read FRegistroC700Count write FRegistroC700Count;
    property RegistroC705Count: Integer read FRegistroC705Count write FRegistroC705Count;
    property RegistroC710Count: Integer read FRegistroC710Count write FRegistroC710Count;
    property RegistroC715Count: Integer read FRegistroC715Count write FRegistroC715Count;
    property RegistroC720Count: Integer read FRegistroC720Count write FRegistroC720Count;
    property RegistroC750Count: Integer read FRegistroC750Count write FRegistroC750Count;
    property RegistroC755Count: Integer read FRegistroC755Count write FRegistroC755Count;
    property RegistroC770Count: Integer read FRegistroC770Count write FRegistroC770Count;
    property RegistroC775Count: Integer read FRegistroC775Count write FRegistroC775Count;
  end;

implementation

uses
  ACBrTXTUtils;

{ TBloco_C }

constructor TBloco_C.Create;
begin
  inherited;
  CriaRegistros;
end;

procedure TBloco_C.CriaRegistros;
begin
  FRegistroC001 := TRegistroC001.Create;
  FRegistroC990 := TRegistroC990.Create;

  FRegistroC020Count := 0;
  FRegistroC030Count := 0;
  FRegistroC035Count := 0;
  FRegistroC040Count := 0;
  FRegistroC250Count := 0;
  FRegistroC255Count := 0;
  FRegistroC260Count := 0;
  FRegistroC265Count := 0;
  FRegistroC300Count := 0;
  FRegistroC305Count := 0;
  FRegistroC310Count := 0;
  FRegistroC315Count := 0;
  FRegistroC320Count := 0;
  FRegistroC325Count := 0;
  FRegistroC500Count := 0;
  FRegistroC550Count := 0;
  FRegistroC555Count := 0;
  FRegistroC560Count := 0;
  FRegistroC570Count := 0;
  FRegistroC575Count := 0;
  FRegistroC600Count := 0;
  FRegistroC605Count := 0;
  FRegistroC610Count := 0;
  FRegistroC615Count := 0;
  FRegistroC620Count := 0;
  FRegistroC625Count := 0;
  FRegistroC700Count := 0;
  FRegistroC705Count := 0;
  FRegistroC710Count := 0;
  FRegistroC715Count := 0;
  FRegistroC720Count := 0;
  FRegistroC750Count := 0;
  FRegistroC755Count := 0;
  FRegistroC770Count := 0;
  FRegistroC775Count := 0;

  FRegistroC990.QTD_LIN_C := 0;
end;

destructor TBloco_C.Destroy;
begin
  LiberaRegistros;
  inherited;
end;

procedure TBloco_C.LiberaRegistros;
begin
  FRegistroC001.Free;
  FRegistroC990.Free;
end;

procedure TBloco_C.LimpaRegistros;
begin
  /// Limpa os Registros
  LiberaRegistros;
  Conteudo.Clear;

  /// Recriar os Registros Limpos
  CriaRegistros;
end;

function TBloco_C.RegistroC001New: TRegistroC001;
begin
  Result := FRegistroC001;
end;

function TBloco_C.RegistroC020New: TRegistroC020;
begin
  Result := FRegistroC001.RegistroC020.New(FRegistroC001);
end;

function TBloco_C.RegistroC030New: TRegistroC030;
var
  C020: TRegistroC020;
begin
  with FRegistroC001.RegistroC020 do
  begin
    C020 := Items[ AchaUltimoPai('C020', 'C030') ];
    Result := C020.RegistroC030.New(C020);
  end;
end;

function TBloco_C.RegistroC035New: TRegistroC035;
var
  C020: TRegistroC020;
  C030: TRegistroC030;
begin
  with FRegistroC001.RegistroC020 do
    C020 := Items[ AchaUltimoPai('C020', 'C030') ];
  with C020.RegistroC030 do
    C030 := Items[ AchaUltimoPai('C030', 'C035') ];
  Result := C030.RegistroC035.New(C030);
end;

function TBloco_C.RegistroC040New: TRegistroC040;
var
  C020: TRegistroC020;
begin
  with FRegistroC001.RegistroC020 do
  begin
    C020 := Items[ AchaUltimoPai('C020', 'C040') ];
    Result := C020.RegistroC040;
  end;
end;

function TBloco_C.RegistroC250New: TRegistroC250;
var
  C020: TRegistroC020;
begin
  with FRegistroC001.RegistroC020 do
    C020 := Items[ AchaUltimoPai('C020', 'C250') ];
  Result := C020.RegistroC250;
end;

function TBloco_C.RegistroC255New: TRegistroC255;
var
  C020: TRegistroC020;
begin
  with FRegistroC001.RegistroC020 do
    C020 := Items[ AchaUltimoPai('C020', 'C250') ];
  Result := C020.RegistroC250.RegistroC255;
end;

function TBloco_C.RegistroC260New: TRegistroC260;
var
  C020: TRegistroC020;
  C250: TRegistroC250;
begin
  with FRegistroC001.RegistroC020 do
    C020 := Items[ AchaUltimoPai('C020', 'C250') ];

  C250 := C020.RegistroC250;
  Result := C250.RegistroC260.New(C250);
end;

function TBloco_C.RegistroC265New: TRegistroC265;
begin

end;

function TBloco_C.RegistroC300New: TRegistroC300;
var
  C020: TRegistroC020;
begin
  with FRegistroC001.RegistroC020 do
    C020 := Items[ AchaUltimoPai('C020', 'C300') ];
  Result := C020.RegistroC300.New(C020);
end;

function TBloco_C.RegistroC305New: TRegistroC305;
var
  C020: TRegistroC020;
  C300: TRegistroC300;
begin
  with FRegistroC001.RegistroC020 do
    C020 := Items[ AchaUltimoPai('C020', 'C300') ];
  with C020.RegistroC300 do
    C300 := Items[ AchaUltimoPai('C300', 'C305') ];
  Result := C300.RegistroC305;
end;

function TBloco_C.RegistroC310New: TRegistroC310;
var
  C020: TRegistroC020;
  C300: TRegistroC300;
begin
  with FRegistroC001.RegistroC020 do
    C020 := Items[ AchaUltimoPai('C020', 'C300') ];
  with C020.RegistroC300 do
    C300 := Items[ AchaUltimoPai('C300', 'C310') ];
  Result := C300.RegistroC310;
end;

function TBloco_C.RegistroC315New: TRegistroC315;
var
  C020: TRegistroC020;
  C300: TRegistroC300;
begin
  with FRegistroC001.RegistroC020 do
    C020 := Items[ AchaUltimoPai('C020', 'C300') ];
  with C020.RegistroC300 do
    C300 := Items[ AchaUltimoPai('C300', 'C315') ];
  Result := C300.RegistroC315;
end;

function TBloco_C.RegistroC320New: TRegistroC320;
var
  C020: TRegistroC020;
  C300: TRegistroC300;
begin
  with FRegistroC001.RegistroC020 do
    C020 := Items[ AchaUltimoPai('C020', 'C300') ];
  with C020.RegistroC300 do
    C300 := Items[ AchaUltimoPai('C300', 'C320') ];
  Result := C300.RegistroC320;
end;

function TBloco_C.RegistroC325New: TRegistroC325;
var
  C020: TRegistroC020;
  C300: TRegistroC300;
begin
  with FRegistroC001.RegistroC020 do
    C020 := Items[ AchaUltimoPai('C020', 'C300') ];
  with C020.RegistroC300 do
    C300 := Items[ AchaUltimoPai('C300', 'C325') ];
  Result := C300.RegistroC325;
end;

function TBloco_C.RegistroC550New: TRegistroC550;
begin
  Result := FRegistroC001.RegistroC550.New(FRegistroC001);
end;

function TBloco_C.RegistroC555New: TRegistroC555;
var
  C550: TRegistroC550;
begin
  with FRegistroC001.RegistroC550 do
    C550 := Items[ AchaUltimoPai('C550', 'C555') ];
  Result := C550.RegistroC555.New(C550);
end;

function TBloco_C.RegistroC560New: TRegistroC560;
var
  C550: TRegistroC550;
begin
  with FRegistroC001.RegistroC550 do
    C550 := Items[ AchaUltimoPai('C550', 'C560') ];
 // Result := C550.RegistroC560.New(C550);
end;

function TBloco_C.RegistroC570New: TRegistroC570;
begin
  Result := FRegistroC001.RegistroC570.New(FRegistroC001);
end;

function TBloco_C.RegistroC575New: TRegistroC575;
var
  C570: TRegistroC570;
begin
  with FRegistroC001.RegistroC570 do
    C570 := Items[ AchaUltimoPai('C570', 'C575')];
  Result := C570.RegistroC575.New(C570);
end;

function TBloco_C.RegistroC600New: TRegistroC600;
begin
  Result := FRegistroC001.RegistroC600.New(FRegistroC001);
end;

function TBloco_C.RegistroC605New: TRegistroC605;
var
  C600: TRegistroC600;
begin
  with FRegistroC001.RegistroC600 do
    C600 := Items[ AchaUltimoPai('C600', 'C605') ];
  Result := C600.RegistroC605.New(C600);
end;

function TBloco_C.RegistroC610New: TRegistroC610;
var
  C600: TRegistroC600;
begin
  with FRegistroC001.RegistroC600 do
    C600 := Items[ AchaUltimoPai('C600', 'C610') ];
  Result := C600.RegistroC610.New(C600);
end;

function TBloco_C.RegistroC615New: TRegistroC615;
var
  C600: TRegistroC600;
begin
  with FRegistroC001.RegistroC600 do
    C600 := Items[ AchaUltimoPai('C600', 'C610') ];
  with C600.RegistroC610 do
    Result := Items[ AchaUltimoPai('C610', 'C615') ].RegistroC615;
end;

function TBloco_C.RegistroC620New: TRegistroC620;
begin
  Result := FRegistroC001.RegistroC620.New(FRegistroC001);
end;

function TBloco_C.RegistroC625New: TRegistroC625;
var
  C620: TRegistroC620;
begin
  with FRegistroC001.RegistroC620 do
    C620 := Items[ AchaUltimoPai('C620', 'C625') ];
  Result := C620.RegistroC625.New(C620);
end;

function TBloco_C.RegistroC700New: TRegistroC700;
begin
  Result := FRegistroC001.RegistroC700.New(FRegistroC001);
end;

function TBloco_C.RegistroC705New: TRegistroC705;
var
  C700: TRegistroC700;
begin
  with FRegistroC001.RegistroC700 do
    C700 := Items[ AchaUltimoPai('C700', 'C705') ];
  Result := C700.RegistroC705;
end;

function TBloco_C.RegistroC710New: TRegistroC710;
begin

end;

function TBloco_C.RegistroC715New: TRegistroC715;
begin

end;

function TBloco_C.RegistroC720New: TRegistroC720;
begin

end;

function TBloco_C.RegistroC750New: TRegistroC750;
var
  C700: TRegistroC700;
begin
  with FRegistroC001.RegistroC700 do
    C700 := Items[ AchaUltimoPai('C700', 'C750') ];
  Result := C700.RegistroC750.New(C700);
end;

function TBloco_C.RegistroC755New: TRegistroC755;
var
  C700: TRegistroC700;
  C750: TRegistroC750;
begin
  with FRegistroC001.RegistroC700 do
    C700 := Items[ AchaUltimoPai('C700', 'C750') ];
  with C700.RegistroC750 do
    C750 := Items[ AchaUltimoPai('C750', 'C755') ];
  Result := C750.RegistroC755;
end;

function TBloco_C.RegistroC770New: TRegistroC770;
begin
  Result := FRegistroC001.RegistroC770.New(FRegistroC001);
end;

function TBloco_C.RegistroC775New: TRegistroC775;
var
  C770: TRegistroC770;
begin
  with FRegistroC001.RegistroC770 do
    C770 := Items[ AchaUltimoPai('C770', 'C775') ];
  Result := C770.RegistroC775.New(C770);
end;

function TBloco_C.SituacaoDoctoToStr(ASituacao: TACBrlSituacaoDocto): String;
begin
  case ASituacao of
    sdlRegular: Result := '00';
    sdlExtempRegular: Result := '01';
    sdlCancelado: Result := '02';
    sdlCancelamentoDocAnterior: Result := '03';
    sdlCanceladoExtemp: Result := '04';
    sdlDesfazimentoNegocio: Result := '05';
    sdlDocumentoReferenciado: Result := '06';
    sdlRegularSimples: Result := '07';
    sdlExtempRegularSimples: Result := '08';
    sdlLancDoctoregular: Result := '50';
    sdlLancExtempDoctoRegular: Result := '51';
    sdlLancDoctoCancelado: Result := '52';
    sdlLancCancelamentoDocAnterior: Result := '53';
    sdlLancCanceladoExtemp: Result := '54';
    sdlLancDesfazimentoNegocio: Result := '55';
    sdlLancDocumentoReferenciado: Result := '56';
    sdlLancDoctoOutrasSituacoes: Result := '58';
    sdlLancDoctoRepercNevativa: Result := '59';
  end;
end;

procedure TBloco_C.WriteRegistroC001;
begin
  if Assigned(FRegistroC001) then
  begin
    with FRegistroC001 do
    begin
      Add( LFill('C001') +
           LFill(Integer(IND_MOV), 0) );

      if IND_MOV = imlComDados then
      begin
        WriteRegistroC020(FRegistroC001);
        WriteRegistroC550(FRegistroC001);
        WriteRegistroC570(FRegistroC001);
        WriteRegistroC580(FRegistroC001);
        WriteRegistroC600(FRegistroC001);
        WriteRegistroC620(FRegistroC001);
        WriteRegistroC640(FRegistroC001);
        WriteRegistroC700(FRegistroC001);
        WriteRegistroC770(FRegistroC001);
      end;
    end;

    FRegistroC990.QTD_LIN_C := FRegistroC990.QTD_LIN_C + 1;
  end;
end;

procedure TBloco_C.WriteRegistroC020(RegC001: TRegistroC001);
var
  intFor: Integer;
  RegC020: TRegistroC020;
begin
  for intFor := 0 to RegC001.RegistroC020.Count - 1 do
  begin
    RegC020 := RegC001.RegistroC020.Items[intFor];
    with RegC020 do
    begin

      Add( LFill('C020') +
           LFill(Integer(IND_OPER), 1) +
           LFill(Integer(IND_EMIT), 1) +
           LFill(COD_PART) +
           LFill(COD_MOD) +
           LFill(SituacaoDoctoToStr(COD_SIT)) +
           LFill(SER) +
           LFill(NUM_DOC, 0) +
           LFill(DT_DOC) +
           LFill(DT_EMIS) +
           LFill(COD_NAT) +
           LFill(VL_DOC, 2) +
           LFill(Integer(IND_PGTO), 1) +
           LFill(VL_DESC, 2) +
           LFill(VL_MERC, 2) +
           LFill(Integer(IND_FRT), 0) +
           LFill(VL_FRT, 2) +
           LFill(VL_SEG, 2) +
           LFill(VL_OUT_DA, 2) +
           LFill(VL_BC_ICMS, 2) +
           LFill(VL_ICMS, 2) +
           LFill(VL_BC_ST, 2) +
           LFill(VL_ICMS_ST, 2) +
           LFill(VL_IPI, 2) +
           LFill(COD_INF_OBS) );
    end;

    WriteRegistroC030(RegC020);
    WriteRegistroC040(RegC020);
    WriteRegistroC050(RegC020);
    WriteRegistroC060(RegC020);
    WriteRegistroC200(RegC020);
    WriteRegistroC250(RegC020);
    WriteRegistroC300(RegC020);
    WriteRegistroC500(RegC020);

    FRegistroC990.QTD_LIN_C := FRegistroC990.QTD_LIN_C + 1;
  end;

  FRegistroC020Count := FRegistroC020Count + RegC001.RegistroC020.Count;
end;

procedure TBloco_C.WriteRegistroC030(RegC020: TRegistroC020);
begin

end;

procedure TBloco_C.WriteRegistroC035(RegC030: TRegistroC030);
begin

end;

procedure TBloco_C.WriteRegistroC040(RegC020: TRegistroC020);
begin

end;

procedure TBloco_C.WriteRegistroC050(RegC020: TRegistroC020);
begin

end;

procedure TBloco_C.WriteRegistroC060(RegC020: TRegistroC020);
begin

end;

procedure TBloco_C.WriteRegistroC200(RegC020: TRegistroC020);
begin

end;

procedure TBloco_C.WriteRegistroC250(RegC020: TRegistroC020);
begin

end;

procedure TBloco_C.WriteRegistroC255(RegC250: TRegistroC250);
begin

end;

procedure TBloco_C.WriteRegistroC260(RegC250: TRegistroC250);
begin

end;

procedure TBloco_C.WriteRegistroC300(RegC020: TRegistroC020);
var
  intFor: Integer;
  C300: TRegistroC300;
begin
  if Assigned(RegC020.RegistroC300) then
  begin
    for intFor := 0 to RegC020.RegistroC300.Count - 1 do
    begin
      C300 := RegC020.RegistroC300.Items[intFor];
      with C300 do
      begin
        Add( LFill('C300')             +
             LFill(NUM_ITEM, 0)        +
             LFill(COD_ITEM)           +
             DFill(VL_UNIT,3) +
             DFill(QTD, 3)     +
             LFill(UNID)               +
             LFill(VL_ITEM, 2)         +
             LFill(VL_DESC_I, 2)       +
             LFill('0')                +
             LFill(NCM)                +
             LFill('')                 +
             LFill('')                 +
             LFill(CST, 0)             +
             LFill(CFOP, 0)            +
             LFill(VL_BC_ICMS_I, 2)    +
             LFill(ALIQ_ICMS, 2)       +
             LFill(VL_ICMS_I, 2)       +
             LFill(VL_BC_ST_I, 2)      +
             LFill(ALIQ_ST, 2)         +
             LFill(VL_ICMS_ST_I, 2)    +
             LFill('')                 +
             LFill(VL_BC_IPI, 2)       +
             LFill(ALIQ_IPI, 2)        +
             LFill(VL_IPI_I, 2) );




      end;

      WriteRegistroC320(C300);
      WriteRegistroC310(C300);
      WriteRegistroC325(C300);
      WriteRegistroC305(C300);
      WriteRegistroC315(C300);

      FRegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
    end;

    FRegistroC300Count := FRegistroC300Count + RegC020.RegistroC300.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC305(RegC300: TRegistroC300);
begin

end;

procedure TBloco_C.WriteRegistroC310(RegC300: TRegistroC300);
begin

end;

procedure TBloco_C.WriteRegistroC315(RegC300: TRegistroC300);
begin

end;

procedure TBloco_C.WriteRegistroC320(RegC300: TRegistroC300);
begin

end;

procedure TBloco_C.WriteRegistroC325(RegC300: TRegistroC300);
begin

end;

procedure TBloco_C.WriteRegistroC500(RegC020: TRegistroC020);
var
  RegC500: TRegistroC500;
  intFor: Integer;
begin
  for intFor := 0 to RegC020.RegistroC500.Count - 1 do
  begin
    RegC500 := RegC020.RegistroC500.Items[intFor];
    with RegC500 do
    begin
      Add( LFill('C500') +
           LFill(CST) +
           LFill(CFOP) +
           DFill(VL_CONT_P, 2) +
           DFill(VL_BC_ICMS_P,2) +
           LFill(ALIQ_ICMS, 2) +
           LFill(VL_ICMS_P, 2)  +
           LFill(VL_ICMS_ST_P, 2) +
           LFill(VL_IPI_P, 2) );


    end;

    FRegistroC990.QTD_LIN_C := FRegistroC990.QTD_LIN_C + 1;
  end;

  FRegistroC500Count := FRegistroC500Count + RegC020.RegistroC500.Count;
end;

procedure TBloco_C.WriteRegistroC550(RegC001: TRegistroC001);
var
  intFor: Integer;
  RegC550: TRegistroC550;
begin
  for intFor := 0 to RegC001.RegistroC550.Count - 1 do
  begin
    RegC550 := RegC001.RegistroC550.Items[intFor];
    with RegC550 do
    begin
      Add( LFill('C550') +
           LFill(CPF_CONS) +
           LFill(CNPJ_CONS) +
           LFill(COD_MOD) +
           LFill(SituacaoDoctoToStr(COD_SIT)) +
           LFill(SER) +
           LFill(SUB) +
           LFill(NUM_DOC, 0) +
           LFill(DT_DOC) +
           LFill(VL_DOC, 2) +
           LFill(VL_DESC, 2) +
           LFill(VL_MERC, 2) +
           LFill(VL_BC_ICMS, 2) +
           LFill(VL_ICMS, 2) +
           LFill(COD_INF_OBS) );
    end;

    WriteRegistroC555(RegC550);

    FRegistroC990.QTD_LIN_C := FRegistroC990.QTD_LIN_C + 1;
  end;

  FRegistroC550Count := FRegistroC550Count + RegC001.RegistroC550.Count;
end;

procedure TBloco_C.WriteRegistroC555(RegC550: TRegistroC550);
var
  RegC555: TRegistroC555;
  intFor: Integer;
begin
  for intFor := 0 to RegC550.RegistroC555.Count - 1 do
  begin
    RegC555 := RegC550.RegistroC555.Items[intFor];
    with RegC555 do
    begin
      Add( LFill('C555') +
           LFill(NUM_ITEM,0) +
           LFill(COD_ITEM) +
           DFill(VL_UNIT, 3) +
           DFill(QTD,3) +
           LFill(UNID) +
           LFill(VL_ITEM, 2) +
           LFill(VL_DESC_I, 2) +
           LFill(CST) +
           LFill(CFOP) +
           LFill(VL_BC_ICMS_I, 2) +
           LFill(ALIQ_ICMS, 2) +
           LFill(VL_ICMS_I, 2) );
    end;

    FRegistroC990.QTD_LIN_C := FRegistroC990.QTD_LIN_C + 1;
  end;

  FRegistroC555Count := FRegistroC555Count + RegC550.RegistroC555.Count;
end;

procedure TBloco_C.WriteRegistroC560(RegC550: TRegistroC550);
begin

end;

procedure TBloco_C.WriteRegistroC570(RegC001: TRegistroC001);
begin

end;

procedure TBloco_C.WriteRegistroC575(RegC570: TRegistroC570);
begin

end;

procedure TBloco_C.WriteRegistroC580(RegC001: TRegistroC001);
begin

end;

procedure TBloco_C.WriteRegistroC600(RegC001: TRegistroC001);
var
  intFor: Integer;
  RegC600: TRegistroC600;
begin
  for intFor := 0 to RegC001.RegistroC600.Count - 1 do
  begin
    RegC600 := RegC001.RegistroC600.Items[intFor];
    with RegC600 do
    begin
      Check(funChecaCPF(CPF_CONS), '(C-C600) ENTIDADE: O CPF "%s" digitado é inválido!', [CPF_CONS]);
      Check(funChecaCNPJ(CNPJ_CONS), '(C-C600) ENTIDADE: O CNPJ "%s" digitado é inválido!', [CNPJ_CONS]);

      Add( LFill('C600') +
           LFill(CPF_CONS) +
           LFill(CNPJ_CONS) +
           LFill(COD_MOD) +
           LFill(SituacaoDoctoToStr(COD_SIT)) +
           LFill(ECF_CX, 0) +
           LFill(ECF_FAB) +
           LFill(CRO, 0) +
           LFill(CRZ, 0) +
           LFill(NUM_DOC, 0) +
           LFill(DT_DOC) +
           LFill(VL_DOC, 2) +
           LFill(VL_CANC_ISS,2) +
           LFill(VL_CANC_ICMS, 2) +
           LFill(VL_CANC_ISS + VL_CANC_ICMS, 2) +
           LFill(VL_DESC_ISS, 2) +
           LFill(VL_DESC_ICMS, 2) +
           LFill(VL_DESC_ISS + VL_DESC_ICMS, 2) +
           LFill(VL_ACMO_ISS, 2) +
           LFill(VL_ACMO_ICMS, 2) +
           LFill(VL_ACMO_ISS + VL_ACMO_ICMS, 2) +
           LFill(VL_ISS, 2) +
           LFill(VL_BC_ICMS, 2) +
           LFill(VL_ICMS, 2) +
           LFill(VL_ISN, 2) +
           LFill(VL_NT, 2) +
           LFill(VL_ST, 2) );
    end;

    WriteRegistroC605(RegC600);
   // WriteRegistroC610(RegC600);

    FRegistroC990.QTD_LIN_C := FRegistroC990.QTD_LIN_C + 1;
  end;

  FRegistroC600Count := FRegistroC600Count + RegC001.RegistroC600.Count;
end;

procedure TBloco_C.WriteRegistroC605(RegC600: TRegistroC600);
var
  intFor: Integer;
  C605: TRegistroC605;
begin
  for intFor := 0 to RegC600.RegistroC605.Count - 1 do
  begin
    C605 := RegC600.RegistroC605.Items[intFor];
    with C605 do
    begin
      Add( LFill('C605') +
           LFill(NUM_ITEM,0) +
           LFill(COD_ITEM) +
           DFill(VL_UNIT, 3) +
           DFill(QTD,3) +
           DFill(QTD_CANC_I,3) +
           LFill(UNID) +
           LFill(VL_ITEM, 2) +
           LFill(VL_DESC_I, 2) +
           LFill(VL_CANC_I, 2) +
           LFill(VL_ACMO_I, 2) +
           LFill(VL_ISS, 2) +
           LFill(CST) +
           LFill(CFOP) +
           LFill(VL_BC_ICMS_I, 2) +
           LFill(ALIQ_ICMS, 2) +
           LFill(VL_ICMS_I, 2) +
           LFill(VL_ISN_I, 2) +
           LFill(VL_NT_I, 2) +
           LFill(VL_ST_I, 2) );
    end;

    FRegistroC990.QTD_LIN_C := FRegistroC990.QTD_LIN_C + 1;
  end;

  FRegistroC605Count := FRegistroC605Count + RegC600.RegistroC605.Count;
end;

procedure TBloco_C.WriteRegistroC610(RegC600: TRegistroC600);
var
  intFor: Integer;
  C610: TRegistroC610;
begin
  if Assigned(RegC600.RegistroC610) then
  begin
    for intFor := 0 to RegC600.RegistroC610.Count - 1 do
    begin
      C610 := RegC600.RegistroC610.Items[intFor];
      with C610 do
      begin
        Add( LFill('C610') +
             LFill(NUM_ITEM, 0) +
             LFill(COD_ITEM) +
             LFill(UNID) +
             LFill(Double(VL_UNIT), 6) +
             LFill(Double(QTD), 6) +
             LFill(VL_DESC_I, 2) +
             LFill(VL_ACMO_I, 2) +
             LFill(VL_ITEM, 2) +
             LFill(CST) +
             LFill(CFOP, 0) +
             LFill(VL_BC_ICMS_I, 2) +
             LFill(ALIQ_ICMS, 2) +
             LFill(VL_ICMS_I, 2) +
             LFill(VL_ISN_I, 2) +
             LFill(VL_NT_I, 2) +
             LFill(VL_ICMS_ST_I, 2) );
      end;

      WriteRegistroC615(C610);

      FRegistroC990.QTD_LIN_C := FRegistroC990.QTD_LIN_C + 1;
    end;

    FRegistroC610Count := FRegistroC610Count + RegC600.RegistroC610.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC615(RegC610: TRegistroC610);
begin

end;

procedure TBloco_C.WriteRegistroC620(RegC001: TRegistroC001);
begin

end;

procedure TBloco_C.WriteRegistroC625(RegC620: TRegistroC620);
begin

end;

procedure TBloco_C.WriteRegistroC640(RegC001: TRegistroC001);
begin

end;

procedure TBloco_C.WriteRegistroC700(RegC001: TRegistroC001);
begin

end;

procedure TBloco_C.WriteRegistroC705(RegC700: TRegistroC700);
begin

end;

procedure TBloco_C.WriteRegistroC750(RegC700: TRegistroC700);
begin

end;

procedure TBloco_C.WriteRegistroC760(RegC700: TRegistroC700);
begin

end;

procedure TBloco_C.WriteRegistroC770(RegC001: TRegistroC001);
begin

end;

procedure TBloco_C.WriteRegistroC775(RegC770: TRegistroC770);
begin

end;

procedure TBloco_C.WriteRegistroC780(RegC770: TRegistroC770);
begin

end;

procedure TBloco_C.WriteRegistroC990;
begin
  if Assigned(FRegistroC990) then
    with FRegistroC990 do
    begin
      QTD_LIN_C := QTD_LIN_C + 1;

      Add( LFill('C990') +
           LFill(QTD_LIN_C, 0) );
    end;
end;

end.
