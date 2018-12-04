{ ****************************************************************************** }
{ Projeto: Componentes ACBr }
{ Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil }
{ }
{ Direitos Autorais Reservados (c) 2009   Isaque Pinheiro }
{ }
{ Colaboradores nesse arquivo: }
{ }
{ Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr }
{ }
{ Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior. }
{ }
{ Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor }
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT) }
{ }
{ Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto }
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc., }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA. }
{ Você também pode obter uma copia da licença em: }
{ http://www.opensource.org/licenses/lgpl-license.php }
{ }
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br }
{ Praça Anita Costa, 34 - Tatuí - SP - 18270-410 }
{ }
{ ****************************************************************************** }

{ ******************************************************************************
  |* Historico
  |*
  |* 06/11/2018: Renan Eustaquio
  |*  - Criação e distribuição da Primeira Versao
  ******************************************************************************* }

unit ACBrEFDBloco_B_Class;

interface

uses SysUtils, Classes, DateUtils, ACBrSped, ACBrEFDBloco_B,
  ACBrEFDBloco_0_Class, ACBrEFDBlocos;

type
  /// TBLOCO_B -
  TBloco_B = class(TACBrSPED)
  private
    FBloco_0: TBloco_0;

    FRegistroB001: TRegistroB001;
    /// BLOCO B - RegistroB001
    FRegistroB990: TRegistroB990;
    /// BLOCO B - RegistroB990

    FRegistroB020Count: Integer;
    FRegistroB025Count: Integer;
    FRegistroB030Count: Integer;
    FRegistroB035Count: Integer;
    FRegistroB350Count: Integer;
    FRegistroB420Count: Integer;
    FRegistroB440Count: Integer;
    FRegistroB460Count: Integer;
    FRegistroB470Count: Integer;
    FRegistroB500Count: Integer;
    FRegistroB510Count: Integer;

    procedure WriteRegistroB020(RegB001: TRegistroB001);
    procedure WriteRegistroB025(RegB020: TRegistroB020);
    procedure WriteRegistroB030(RegB001: TRegistroB001);
    procedure WriteRegistroB035(RegB030: TRegistroB030);
    procedure WriteRegistroB350(RegB001: TRegistroB001);
    procedure WriteRegistroB420(RegB001: TRegistroB001);
    procedure WriteRegistroB440(RegB001: TRegistroB001);
    procedure WriteRegistroB460(RegB001: TRegistroB001);
    procedure WriteRegistroB470(RegB001: TRegistroB001);
    procedure WriteRegistroB500(RegB001: TRegistroB001);
    procedure WriteRegistroB510(RegB500: TRegistrob500);

    procedure CriaRegistros;
    procedure LiberaRegistros;
  public
    constructor Create;
    destructor Destroy; override;
    /// Destroy
    procedure LimpaRegistros; override;

    function RegistroB001New: TRegistroB001;
    function RegistroB020New: TRegistroB020;
    function RegistroB025New: TRegistroB025;
    function RegistroB030New: TRegistroB030;
    function RegistroB035New: TRegistroB035;
    function RegistroB350New: TRegistroB350;
    function RegistroB420New: TRegistroB420;
    function RegistroB440New: TRegistroB440;
    function RegistroB460New: TRegistroB460;
    function RegistroB470New: TRegistroB470;
    function RegistroB500New: TRegistrob500;
    function RegistroB510New: TRegistroB510;

    procedure WriteRegistroB001;
    procedure WriteRegistroB990;

    property Bloco_0: TBloco_0 read FBloco_0 write FBloco_0;
    property RegistroB001: TRegistroB001 read FRegistroB001 write FRegistroB001;
    property RegistroB990: TRegistroB990 read FRegistroB990 write FRegistroB990;

    property RegistroB020Count: Integer read FRegistroB020Count write FRegistroB020Count;
    property RegistroB025Count: Integer read FRegistroB025Count write FRegistroB025Count;
    property RegistroB030Count: Integer read FRegistroB030Count write FRegistroB030Count;
    property RegistroB035Count: Integer read FRegistroB035Count write FRegistroB035Count;
    property RegistroB350Count: Integer read FRegistroB350Count write FRegistroB350Count;
    property RegistroB420Count: Integer read FRegistroB420Count write FRegistroB420Count;
    property RegistroB440Count: Integer read FRegistroB440Count write FRegistroB440Count;
    property RegistroB460Count: Integer read FRegistroB460Count write FRegistroB460Count;
    property RegistroB470Count: Integer read FRegistroB470Count write FRegistroB470Count;
    property RegistroB500Count: Integer read FRegistroB500Count write FRegistroB500Count;
    property RegistroB510Count: Integer read FRegistroB510Count write FRegistroB510Count;
  end;

implementation

uses ACBrSpedFiscal, ACBrTXTUtils, ACBrUtil, strutils;

{ TBloco_C }

constructor TBloco_B.Create;
begin
  inherited;
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
  FRegistroB350Count := 0;
  FRegistroB420Count := 0;
  FRegistroB440Count := 0;
  FRegistroB460Count := 0;
  FRegistroB470Count := 0;
  FRegistroB500Count := 0;
  FRegistroB510Count := 0;

  FRegistroB990.QTD_LIN_B := 0;
end;

procedure TBloco_B.LiberaRegistros;
begin
  FRegistroB001.Free;
  FRegistroB990.Free;
end;

procedure TBloco_B.LimpaRegistros;
begin
  LiberaRegistros;
  Conteudo.Clear;
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
begin
  Result := FRegistroB001.RegistroB020.Items[FRegistroB001.RegistroB020.Count - 1].RegistroB025.New;
end;

function TBloco_B.RegistroB030New: TRegistroB030;
begin
  Result := FRegistroB001.RegistroB030.New(FRegistroB001);
end;

function TBloco_B.RegistroB035New: TRegistroB035;
begin
  Result := FRegistroB001.RegistroB030.Items[FRegistroB001.RegistroB030.Count - 1].RegistroB035.New;
end;

function TBloco_B.RegistroB350New: TRegistroB350;
begin
  Result := FRegistroB001.RegistroB350.New(FRegistroB001);
end;

function TBloco_B.RegistroB420New: TRegistroB420;
begin
  Result := FRegistroB001.RegistroB420.New(FRegistroB001);
end;

function TBloco_B.RegistroB440New: TRegistroB440;
begin
  Result := FRegistroB001.RegistroB440.New(FRegistroB001);
end;

function TBloco_B.RegistroB460New: TRegistroB460;
begin
  Result := FRegistroB001.RegistroB460.New(FRegistroB001);
end;

function TBloco_B.RegistroB470New: TRegistroB470;
begin
  Result := FRegistroB001.RegistroB470.New(FRegistroB001);
end;

function TBloco_B.RegistroB500New: TRegistrob500;
begin
  Result := FRegistroB001.RegistroB500.New(FRegistroB001);
end;

function TBloco_B.RegistroB510New: TRegistroB510;
begin
  Result := FRegistroB001.RegistroB500.Items[FRegistroB001.RegistroB500.Count - 1].RegistroB510.New;
end;

procedure TBloco_B.WriteRegistroB020(RegB001: TRegistroB001);
var
  intFor: integer;
begin
  if Assigned( RegB001.RegistroB020 ) then
  begin
    for intFor := 0 to RegB001.RegistroB020.Count - 1 do
    begin
      with RegB001.RegistroB020.Items[intFor] do
      begin
        Add( LFill('B020') +
             LFill( Integer(IND_OPER), 0 ) +
             LFill( Integer(IND_EMIT), 0 ) +
             LFill( COD_PART ) +
             LFill( COD_MOD  ) +
             LFill( CodSitToStr(COD_SIT)  ) +
             LFill( SER  ) +
             LFill( NUM_DOC  ) +
             LFill( CHV_NFE  ) +
             LFill( DT_DOC, 'ddmmyyyy' ) +
             LFill( COD_MUN_SERV ) +
             LFill( VL_CONT,0,2, True ) +
             LFill( VL_MAT_TERC,0,2, True ) +
             LFill( VL_SUB,0,2, True ) +
             LFill( VL_ISNT_ISS,0,2, True ) +
             LFill( VL_DED_BC,0,2, True ) +
             LFill( VL_BC_ISS,0,2, true ) +
             LFill( VL_BC_ISS_RT,0,2, true ) +
             LFill( VL_ISS_RT,0,2, true ) +
             LFill( VL_ISS ,0,2, true ) +
             LFill( COD_INF_OBS)) ;
      end;
      /// Registro FILHO
      WriteRegistroB025( RegB001.RegistroB020.Items[intFor] ) ;
      RegistroB990.QTD_LIN_B := RegistroB990.QTD_LIN_B + 1;
    end;
    /// Variavél para armazenar a quantidade de registro do tipo.
    FRegistroB020Count := FRegistroB020Count + RegB001.RegistroB020.Count;
    RegB001.RegistroB020.Clear;
  end;
end;

procedure TBloco_B.WriteRegistroB025(RegB020: TRegistroB020);
var
  intFor: integer;
begin
  if Assigned( RegB020.RegistroB025 ) then
  begin
     for intFor := 0 to RegB020.RegistroB025.Count - 1 do
     begin
        with RegB020.RegistroB025.Items[intFor] do
        begin
          Add( LFill('B025') +
               LFill( VL_CONT_P, 0, 2 ) +
               LFill( VL_BC_ISS_P, 0, 2 ) +
               LFill( ALIQ_ISS, 0, 2 ) +
               LFill( VL_ISS_P, 0, 2 ) +
               LFill( VL_ISNT_ISS_P, 0, 2 ) +
               LFill( COD_SERV ));
        end;
        RegistroB990.QTD_LIN_B := RegistroB990.QTD_LIN_B + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroB025Count := FRegistroB025Count + RegB020.RegistroB025.Count;
  end;
end;

procedure TBloco_B.WriteRegistroB030(RegB001: TRegistroB001);
var
  intFor: integer;
begin
  if Assigned( RegB001.RegistroB030 ) then
  begin
    for intFor := 0 to RegB001.RegistroB030.Count - 1 do
    begin
      with RegB001.RegistroB030.Items[intFor] do
      begin
        Add( LFill('B030') +
             LFill( COD_MOD  ) +
             LFill( SER  ) +
             LFill( NUM_DOC_INI  ) +
             LFill( NUM_DOC_FIN  ) +
             LFill( DT_DOC, 'ddmmyyyy' ) +
             LFill( QTD_CANC,0 ) +
             LFill( VL_CONT,0,2, True ) +
             LFill( VL_ISNT_ISS,0,2, True ) +
             LFill( VL_BC_ISS,0,2, true ) +
             LFill( VL_ISS ,0,2, true ) +
             LFill( COD_INF_OBS)) ;
      end;
      /// Registro FILHO
      WriteRegistroB035( RegB001.RegistroB030.Items[intFor] ) ;
      RegistroB990.QTD_LIN_B := RegistroB990.QTD_LIN_B + 1;
    end;
    /// Variavél para armazenar a quantidade de registro do tipo.
    FRegistroB030Count := FRegistroB030Count + RegB001.RegistroB030.Count;
    RegB001.RegistroB030.Clear;
  end;
end;

procedure TBloco_B.WriteRegistroB035(RegB030: TRegistroB030);
var
  intFor: integer;
begin
  if Assigned( RegB030.RegistroB035 ) then
  begin
    for intFor := 0 to RegB030.RegistroB035.Count - 1 do
    begin
      with RegB030.RegistroB035.Items[intFor] do
      begin
        Add( LFill('B035') +
             LFill( VL_CONT_P, 0, 2 ) +
             LFill( VL_BC_ISS_P, 0, 2 ) +
             LFill( ALIQ_ISS, 0, 2 ) +
             LFill( VL_ISS_P, 0, 2 ) +
             LFill( VL_ISNT_ISS_P, 0, 2 ) +
             LFill( COD_SERV ));
      end;
      RegistroB990.QTD_LIN_B := RegistroB990.QTD_LIN_B + 1;
    end;
    /// Variavél para armazenar a quantidade de registro do tipo.
    FRegistroB025Count := FRegistroB025Count + RegB030.RegistroB035.Count;
  end;
end;

procedure TBloco_B.WriteRegistroB350(RegB001: TRegistroB001);
var
  intFor: integer;
begin
  if Assigned( RegB001.RegistroB350 ) then
  begin
    for intFor := 0 to RegB001.RegistroB350.Count - 1 do
    begin
      with RegB001.RegistroB350.Items[intFor] do
      begin
        Add( LFill('B035') +
             LFill( COD_CTD ) +
             LFill( CTA_ISS ) +
             LFill( CTA_COSIF,8 ) +
             LFill( QTD_OCOR,0) +
             LFill( COD_SERV,4 ) +
             LFill( VL_CONT, 0, 2 ) +
             LFill( VL_BC_ISS, 0, 2 ) +
             LFill( ALIQ_ISS, 0, 2 ) +
             LFill( VL_ISS, 0, 2 ) +
             LFill( COD_INF_OBS ));
      end;
      RegistroB990.QTD_LIN_B := RegistroB990.QTD_LIN_B + 1;
    end;
    /// Variavél para armazenar a quantidade de registro do tipo.
    FRegistroB350Count := FRegistroB350Count + RegB001.RegistroB350.Count;
  end;
end;

procedure TBloco_B.WriteRegistroB420(RegB001: TRegistroB001);
var
  intFor: integer;
begin
  if Assigned( RegB001.RegistroB420 ) then
  begin
    for intFor := 0 to RegB001.RegistroB420.Count - 1 do
    begin
      with RegB001.RegistroB420.Items[intFor] do
      begin
        Add( LFill('B420') +
             LFill( VL_CONT, 0, 2 ) +
             LFill( VL_BC_ISS, 0, 2 ) +
             LFill( ALIQ_ISS, 0, 2 ) +
             LFill( VL_ISNT_ISS, 0, 2 ) +
             LFill( VL_ISS, 0, 2 ) +
             LFill( COD_SERV ));
      end;
      RegistroB990.QTD_LIN_B := RegistroB990.QTD_LIN_B + 1;
    end;
    /// Variavél para armazenar a quantidade de registro do tipo.
    FRegistroB420Count := FRegistroB420Count + RegB001.RegistroB420.Count;
  end;

end;

procedure TBloco_B.WriteRegistroB440(RegB001: TRegistroB001);
var
  intFor: integer;
begin
  if Assigned( RegB001.RegistroB440 ) then
  begin
    for intFor := 0 to RegB001.RegistroB440.Count - 1 do
    begin
      with RegB001.RegistroB440.Items[intFor] do
      begin
        Add( LFill('B440') +
             LFill( IndOperToStr(IND_OPER),2) +
             LFill( COD_PART) +
             LFill( VL_CONT_RT, 0, 2 ) +
             LFill( VL_BC_ISS_RT, 0, 2 ) +
             LFill( VL_ISS_RT, 0, 2 ));
      end;
      RegistroB990.QTD_LIN_B := RegistroB990.QTD_LIN_B + 1;
    end;
    /// Variavél para armazenar a quantidade de registro do tipo.
    FRegistroB440Count := FRegistroB440Count + RegB001.RegistroB440.Count;
  end;
end;

procedure TBloco_B.WriteRegistroB460(RegB001: TRegistroB001);
var
  intFor: integer;
begin
  if Assigned( RegB001.RegistroB460 ) then
  begin
    for intFor := 0 to RegB001.RegistroB460.Count - 1 do
    begin
      with RegB001.RegistroB460.Items[intFor] do
      begin
        Add( LFill('B460') +
             LFill( IndicadorDeducaoToStr(IND_DED)) +
             LFill( VL_DED,0,2) +
             LFill( NUM_PROC ) +
             LFill( IndicadorProcessoToStr(IND_PROC) ) +
             LFill( PROC ) +
             LFill( COD_INF_OBS )+
             LFill( IndicadorObrigacaoToStr(IND_OBR) ));
      end;
      RegistroB990.QTD_LIN_B := RegistroB990.QTD_LIN_B + 1;
    end;
    /// Variavél para armazenar a quantidade de registro do tipo.
    FRegistroB460Count := FRegistroB460Count + RegB001.RegistroB460.Count;
  end;
end;

procedure TBloco_B.WriteRegistroB470(RegB001: TRegistroB001);
var
  intFor: integer;
begin
  if Assigned( RegB001.RegistroB470 ) then
  begin
    for intFor := 0 to RegB001.RegistroB470.Count - 1 do
    begin
      with RegB001.RegistroB470.Items[intFor] do
      begin
        Add( LFill('B470') +
             LFill( VL_CONT,0,2) +
             LFill( VL_MAT_TERC,0,2) +
             LFill( VL_MAT_PROP,0,2) +
             LFill( VL_ISNT,0,2) +
             LFill( VL_DED_BC,0,2) +
             LFill( VL_BC_ISS,0,2) +
             LFill( VL_BC_ISS_RT,0,2) +
             LFill( VL_ISS,0,2) +
             LFill( VL_ISS_RT,0,2) +
             LFill( VL_DED,0,2) +
             LFill( VL_ISS_REC ,0,2) +
             LFill( VL_ISS_ST ,0,2) +
             LFill( VL_ISS_REC_UNI,0,2));
      end;
      RegistroB990.QTD_LIN_B := RegistroB990.QTD_LIN_B + 1;
    end;
    /// Variavél para armazenar a quantidade de registro do tipo.
    FRegistroB470Count := FRegistroB470Count + RegB001.RegistroB470.Count;
  end;
end;

procedure TBloco_B.WriteRegistroB500(RegB001: TRegistroB001);
var
  intFor: integer;
begin
  if Assigned( RegB001.RegistroB500 ) then
  begin
    for intFor := 0 to RegB001.RegistroB500.Count - 1 do
    begin
      with RegB001.RegistroB500.Items[intFor] do
      begin
        Add( LFill('B500') +
             LFill( VL_REC,0,2) +
             LFill( QTD_PROF,0) +
             LFill( VL_OR,0,2));
      end;
      RegistroB990.QTD_LIN_B := RegistroB990.QTD_LIN_B + 1;
      /// Registro FILHO
      WriteRegistroB510( RegB001.RegistroB500.Items[intFor] ) ;
    end;
    /// Variavél para armazenar a quantidade de registro do tipo.
    FRegistroB500Count := FRegistroB500Count + RegB001.RegistroB500.Count;
  end;
end;

procedure TBloco_B.WriteRegistroB510(RegB500: TRegistrob500);
var
  intFor: integer;
begin
  if Assigned( RegB500.RegistroB510 ) then
  begin
    for intFor := 0 to RegB500.RegistroB510.Count - 1 do
    begin
      with RegB500.RegistroB510.Items[intFor] do
      begin
        Add( LFill('B510') +
             LFill( IND_PROF) +
             LFill( IND_ESC) +
             LFill( IND_SOC) +
             LFill( CPF ) +
             LFill( NOME ));
      end;
      RegistroB990.QTD_LIN_B := RegistroB990.QTD_LIN_B + 1;
    end;
    /// Variavél para armazenar a quantidade de registro do tipo.
    FRegistroB510Count := FRegistroB510Count + RegB500.RegistroB510.Count;
  end;
end;

procedure TBloco_B.WriteRegistroB990;
begin
  if Assigned(RegistroB990) then
  begin
    with RegistroB990 do
    begin
      QTD_LIN_B := QTD_LIN_B + 1;
      Add( LFill('B990') +
           LFill(QTD_LIN_B,0) ) ;
    end;
  end;
end;

procedure TBloco_B.WriteRegistroB001;
begin
  if Assigned(FRegistroB001) then
  begin
    if (RegistroB990.QTD_LIN_B = 0) then
    begin
      with FRegistroB001 do
      begin
        Add(LFill('B001') + LFill(Integer(IND_MOV), 0));
      end;

      RegistroB990.QTD_LIN_B := RegistroB990.QTD_LIN_B + 1;
    end;
    if FBloco_0.Registro0000.UF = 'DF' then
  	begin
  	  if FRegistroB001.IND_MOV = imComDados then
  	  begin
    		WriteRegistroB020(FRegistroB001);
    		WriteRegistroB030(FRegistroB001);
    		WriteRegistroB350(FRegistroB001);
    		WriteRegistroB420(FRegistroB001);
    		WriteRegistroB440(FRegistroB001);
    		WriteRegistroB460(FRegistroB001);
    		WriteRegistroB470(FRegistroB001);
    		WriteRegistroB500(FRegistroB001);
  	  end;
  	end
	else
		FRegistroB001.IND_MOV := imSemDados;
  end;
end;

end.
