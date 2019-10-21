{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2015   João Pedro R Costa                   }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
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
{ Hw Sistemas e Computadores Ltda -  contato@hwsistemas.com.br                 }
{ www.hwsistemas.com.br - joaopedro@hwsistemas.com.br                          }
{              Rua Mogno, 236 - Governado Valadares - MG - 35065-019           }
{                                                                              }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* 28/12/2015: João Pedro
|*  - Criação e distribuição da Primeira Versao
*******************************************************************************}

unit ACBrDeSTDA;

interface

uses
  SysUtils, Math, Classes, ACBrBase,
{$IFNDEF Framework}
  {$IFDEF FPC}
    LResources,
  {$ENDIF}
{$ENDIF}
  DateUtils, ACBRDeSTDA4715, ACBrTXTClass, ACBrDeSTDABloco_0_Class, 
  ACBrDeSTDABloco_G_Class, ACBrDeSTDABloco_9_Class;

type
  { TACBrDeSTDA }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TACBrDeSTDA = class(TACBrComponent)
  private  
    FACBrTXT: TACBrTXTClass;
    FArquivo: ansistring;
    FInicializado : boolean;
    FOnError: TErrorEvent;

//    FEventsBloco_0: TEventsBloco_0;
//    FEventsBloco_G: TEventsBloco_G;

    FDT_INI: TDateTime;           /// Data inicial das informações contidas no arquivo
    FDT_FIN: TDateTime;           /// Data final das informações contidas no arquivo

    FPath: ansistring;            /// Path do arquivo a ser gerado
    FDelimitador: ansistring;     /// Caracter delimitador de campos
    FTrimString: boolean;
    /// Retorna a string sem espaços em branco iniciais e finais
    FCurMascara: ansistring;      /// Mascara para valores tipo currency

    FBloco_0: TBloco_0;
    FBloco_G: TBloco_G;
    FBloco_9: TBloco_9;

    function GetConteudo: TStringList;
    function GetDelimitador: ansistring;
    function GetLinhasBuffer: Integer;
    function GetTrimString: boolean;
    function GetCurMascara: ansistring;
    function GetDT_FIN: TDateTime;
    function GetDT_INI: TDateTime;
    procedure InicializaBloco(Bloco: TACBrDeSTDA4715);
    procedure SetArquivo(const AValue: ansistring);
    procedure SetDelimitador(const Value: ansistring);
    procedure SetLinhasBuffer(const AValue: Integer);
    procedure SetPath(const AValue: ansistring);
    procedure SetTrimString(const Value: boolean);
    procedure SetCurMascara(const Value: ansistring);
    procedure SetDT_FIN(const Value: TDateTime);
    procedure SetDT_INI(const Value: TDateTime);

    function GetOnError: TErrorEvent; /// Método do evento OnError
    procedure SetOnError(const Value: TErrorEvent); /// Método SetError

    procedure LimpaRegistros;
  protected
    /// BLOCO 0
    procedure WriteRegistro0000;
    procedure WriteRegistro0001;
    procedure WriteRegistro0990;
    /// BLOCO G
    procedure WriteRegistroG001;
    procedure WriteRegistroG990;
    /// BLOCO 9
    procedure WriteRegistro9001;
    procedure WriteRegistro9900;
    procedure WriteRegistro9990;
    procedure WriteRegistro9999;
  public
    constructor Create(AOwner: TComponent); override; /// Create
    destructor Destroy; override; /// Destroy

    procedure SaveFileTXT;

    procedure IniciaGeracao;
    procedure CancelaGeracao;
    procedure WriteBloco_0;
    procedure WriteBloco_G( FechaBloco: Boolean );
    procedure WriteBloco_9;

    property Conteudo: TStringList read GetConteudo;

    property DT_INI: TDateTime read GetDT_INI write SetDT_INI;
    property DT_FIN: TDateTime read GetDT_FIN write SetDT_FIN;

    property Bloco_0: TBloco_0 read FBloco_0 write FBloco_0;
    property Bloco_G: TBloco_G read FBloco_G write FBloco_G;
    property Bloco_9: TBloco_9 read FBloco_9 write FBloco_9;
  published
    property Path: ansistring read FPath write SetPath;
    property Arquivo: ansistring read FArquivo write SetArquivo;
    property LinhasBuffer : Integer read GetLinhasBuffer write SetLinhasBuffer
      default 1000 ;

    ///
    property Delimitador: ansistring read GetDelimitador write SetDelimitador;
    property TrimString: boolean read GetTrimString write SetTrimString;
    property CurMascara: ansistring read GetCurMascara write SetCurMascara;

    property OnError: TErrorEvent read GetOnError write SetOnError;

//    property EventsBloco_0: TEventsBloco_0 read FEventsBloco_0;
//    property EventsBloco_G: TEventsBloco_G read FEventsBloco_G;

  end;

procedure Register;


implementation

uses ACBrUtil, ACBrDeSTDABlocos, ACBrDeSTDABloco_G, ACBrDeSTDABloco_9;

{$IFNDEF FPC}
 {$R ACBr_DeSTDA.dcr}
{$ENDIF}

procedure Register;
begin
  RegisterComponents('ACBrTXT', [TACBrDeSTDA]);
end;

{ TACBrDeSTDA }

procedure TACBrDeSTDA.CancelaGeracao;
begin
  LimpaRegistros;
  FInicializado := False;
end;

constructor TACBrDeSTDA.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FACBrTXT := TACBrTXTClass.Create;
  FACBrTXT.LinhasBuffer := 1000 ;

  FInicializado := False;

  FBloco_0 := TBloco_0.Create;
  FBloco_G := TBloco_G.Create;
  FBloco_9 := TBloco_9.Create;

  FPath := ExtractFilePath(ParamStr(0));
  FDelimitador := '|';
  FCurMascara := '#0.00';
  FTrimString := True;

  // Seta os valores defaults para todos os cdaBlocos
  SetDelimitador(FDelimitador);
  SetCurMascara(FCurMascara);
  SetTrimString(FTrimString);

end;

destructor TACBrDeSTDA.Destroy;
begin
  FACBrTXT.Free;

  FBloco_0.Free;
  FBloco_G.Free;
  FBloco_9.Free;

  inherited;
end;

function TACBrDeSTDA.GetConteudo: TStringList;
begin
  Result := FACBrTXT.Conteudo;
end;

function TACBrDeSTDA.GetCurMascara: ansistring;
begin
  Result := FCurMascara;
end;

function TACBrDeSTDA.GetDelimitador: ansistring;
begin
   Result := FDelimitador;
end;

function TACBrDeSTDA.GetDT_FIN: TDateTime;
begin
  Result := FDT_FIN;
end;

function TACBrDeSTDA.GetDT_INI: TDateTime;
begin
  Result := FDT_INI;
end;

function TACBrDeSTDA.GetLinhasBuffer: Integer;
begin
   Result := FACBrTXT.LinhasBuffer;
end;

function TACBrDeSTDA.GetOnError: TErrorEvent;
begin
  Result := FOnError;
end;

function TACBrDeSTDA.GetTrimString: boolean;
begin
  Result := FTrimString;
end;

procedure TACBrDeSTDA.IniciaGeracao;
begin
  if FInicializado then exit ;

  if (Trim(FArquivo) = '') or (Trim(FPath) = '') then
    raise EACBrDeSTDAException.Create(ACBrStr('Caminho ou nome do arquivo não informado!'));

  FACBrTXT.NomeArquivo := FPath + FArquivo ;
  FACBrTXT.Reset;    // Apaga o Arquivo e limpa memória

  InicializaBloco( Bloco_0 ) ;
  InicializaBloco( Bloco_G ) ;
  InicializaBloco( Bloco_9 ) ;

  ///
  FACBrTXT.Check(FDT_INI > 0, 'CHECAGEM INICIAL: Informe a data '
    + 'inicial das informações contidas no arquivo!');
  FACBrTXT.Check(FDT_FIN > 0, 'CHECAGEM INICIAL: Informe a data '
    + 'final das informações contidas no arquivo!');
  FACBrTXT.Check(DayOf(FDT_INI) = 1, 'CHECAGEM INICIAL: A data inicial deve '
    + 'corresponder ao primeiro dia do mês informado!');
  FACBrTXT.Check(FDT_FIN >= FDT_INI, 'CHECAGEM INICIAL: A data final deve se '
    + 'maior que a data inicial!');
  FACBrTXT.Check(FDT_FIN <= Date, 'CHECAGEM INICIAL: A data final "%s" '
    + 'não pode ser superior a data atual "%s"!',
    [DateToStr(FDT_FIN), DateToStr(Date)]);
  FACBrTXT.Check(DateOf(EndOfTheMonth(FDT_FIN)) = DateOf(FDT_FIN),
    'CHECAGEM ' + 'INICIAL: A data final deve corresponder ao último dia do mês '
    + 'informado!');

  /// Preparação para totalizações de registros.
  Bloco_0.Registro0990.QTD_LIN_0 := 0;
  Bloco_G.RegistroG990.QTD_LIN_G := 0;
  Bloco_9.Registro9990.QTD_LIN_9 := 0;
  Bloco_9.Registro9999.QTD_LIN   := 0;

  /// Limpa a lista
  Bloco_9.Registro9900.Clear;

  FInicializado := True ;
end;

procedure TACBrDeSTDA.InicializaBloco(Bloco: TACBrDeSTDA4715);
begin
   Bloco.NomeArquivo  := FACBrTXT.NomeArquivo;
   Bloco.LinhasBuffer := FACBrTXT.LinhasBuffer;
   Bloco.Gravado      := False ;
   Bloco.Conteudo.Clear;
end;

procedure TACBrDeSTDA.LimpaRegistros;
begin
  FBloco_0.LimpaRegistros;
  FBloco_G.LimpaRegistros;
  FBloco_9.LimpaRegistros;
end;

procedure TACBrDeSTDA.SaveFileTXT;
begin
  try
    IniciaGeracao;

    WriteBloco_0;
    WriteBloco_G( True );    // True = Fecha o Bloco
    WriteBloco_9;
  finally
    /// Limpa de todos os Blocos as listas de todos os registros.
    LimpaRegistros;
    FACBrTXT.Conteudo.Clear;

    FInicializado := False ;
  end;
end;

procedure TACBrDeSTDA.SetArquivo(const AValue: ansistring);
var
  APath : AnsiString;
begin
  if FArquivo = AValue then
     exit;

  FArquivo := ExtractFileName( AValue );
  APath    := ExtractFilePath( AValue );

  if APath <> '' then
     Path := APath;
end;

procedure TACBrDeSTDA.SetCurMascara(const Value: ansistring);
begin
  FCurMascara := Value;

  FBloco_0.CurMascara := Value;
  FBloco_G.CurMascara := Value;
  FBloco_9.CurMascara := Value;
end;

procedure TACBrDeSTDA.SetDelimitador(const Value: ansistring);
begin
  FDelimitador := Value;

  FBloco_0.Delimitador := Value;
  FBloco_G.Delimitador := Value;
  FBloco_9.Delimitador := Value;
end;

procedure TACBrDeSTDA.SetDT_FIN(const Value: TDateTime);
begin
  FDT_FIN := Value;

  FBloco_0.DT_FIN := Value;
  FBloco_G.DT_FIN := Value;
  FBloco_9.DT_FIN := Value;

  if Assigned(FBloco_0) then
  begin
    FBloco_0.Registro0000.DT_FIN := Value;
    {FBloco_E.RegistroE100.DT_FIN := Value;}
  end;
end;

procedure TACBrDeSTDA.SetDT_INI(const Value: TDateTime);
begin
  FDT_INI := Value;

  FBloco_0.DT_INI := Value;
  FBloco_G.DT_INI := Value;
  FBloco_9.DT_INI := Value;

  if Assigned(FBloco_0) then
  begin
    FBloco_0.Registro0000.DT_INI := Value;
    {FBloco_E.RegistroE100.DT_INI := Value;}
  end;
end;

procedure TACBrDeSTDA.SetLinhasBuffer(const AValue: Integer);
begin
 FACBrTXT.LinhasBuffer := AValue;
end;

procedure TACBrDeSTDA.SetOnError(const Value: TErrorEvent);
begin
  FOnError := Value;

  FBloco_0.OnError := Value;
  FBloco_G.OnError := Value;
  FBloco_9.OnError := Value;
end;

procedure TACBrDeSTDA.SetPath(const AValue: ansistring);
begin
  FPath := PathWithDelim( AValue );
end;

procedure TACBrDeSTDA.SetTrimString(const Value: boolean);
begin
  FTrimString := Value;

  FBloco_0.TrimString := Value;
  FBloco_G.TrimString := Value;
  FBloco_9.TrimString := Value;
end;

procedure TACBrDeSTDA.WriteBloco_0;
begin
  if Bloco_0.Gravado then exit ;

  if not FInicializado then
     raise EACBrDeSTDAException.Create( 'Métodos "IniciaGeracao" não foi executado' );

  // BLOCO 0
  WriteRegistro0000;
  WriteRegistro0001;
  WriteRegistro0990;
  Bloco_0.WriteBuffer;
  Bloco_0.Conteudo.Clear;
  Bloco_0.Gravado := True;
end;

procedure TACBrDeSTDA.WriteBloco_9;
begin
   if Bloco_9.Gravado then exit ;

   if not Bloco_G.Gravado then
      WriteBloco_G ( True ) ;

   /// BLOCO 9
   WriteRegistro9001;
   WriteRegistro9900;
   WriteRegistro9990;
   WriteRegistro9999;
   Bloco_9.WriteBuffer;
   Bloco_9.Conteudo.Clear;
   Bloco_9.Gravado := True ;
end;

procedure TACBrDeSTDA.WriteBloco_G(FechaBloco: Boolean);
begin
   if Bloco_G.Gravado then exit ;

   if not Bloco_0.Gravado then
      WriteBloco_0 ;

   /// BLOCO G
   WriteRegistroG001;

   if Bloco_G.RegistroG001.IND_MOV = imSemDados then
      FechaBloco := True ;

   if FechaBloco then
      WriteRegistroG990;

   Bloco_G.WriteBuffer;
   Bloco_G.Conteudo.Clear;

   Bloco_G.Gravado := FechaBloco;
end;

procedure TACBrDeSTDA.WriteRegistro0000;
begin
   with Bloco_9.Registro9900.New do
   begin
      REG_BLC := '0000';
      QTD_REG_BLC := 1;
   end;
   Bloco_0.WriteRegistro0000;
end;

procedure TACBrDeSTDA.WriteRegistro0001;
begin
  // Preenche as classes com os dados
  Bloco_0.WriteRegistro0001;
  //
  with Bloco_9.Registro9900 do
  begin
    with New do
    begin
      REG_BLC := '0001';
      QTD_REG_BLC := 1;
    end;
  end;
  if Bloco_0.Registro0001.IND_MOV = imComDados then
  begin
    with Bloco_9.Registro9900 do
    begin
      if Bloco_0.Registro0002Count > 0 then
      begin
        with New do
        begin
          REG_BLC := '0002';
          QTD_REG_BLC := Bloco_0.Registro0002Count;
        end;
      end;
      with New do
      begin
        REG_BLC := '0005';
        QTD_REG_BLC := 1;
      end;
      with New do
      begin
        REG_BLC := '0030';
        QTD_REG_BLC := 1;
      end;
      with New do
      begin
        REG_BLC := '0100';
        QTD_REG_BLC := 1;
      end;
    end;
  end;
end;

procedure TACBrDeSTDA.WriteRegistro0990;
begin
  with Bloco_9.Registro9900.New do
  begin
    REG_BLC := '0990';
    QTD_REG_BLC := 1;
  end;
  Bloco_0.WriteRegistro0990;
end;

procedure TACBrDeSTDA.WriteRegistro9001;
begin
  with Bloco_9.Registro9900.New do
  begin
     REG_BLC    := '9001';
     QTD_REG_BLC:= 1;
  end;
  Bloco_9.WriteRegistro9001;

  if Bloco_9.Registro9001.IND_MOV = imComDados then
  begin
    with Bloco_9.Registro9900 do
    begin
      if Bloco_9.Registro9020Count > 0 then
      begin
        with New do
        begin
          REG_BLC := '9020';
          QTD_REG_BLC := Bloco_9.Registro9020Count;
        end;
      end;
      
      if Bloco_9.Registro9030Count > 0 then
      begin
        with New do
        begin
          REG_BLC := '9030';
          QTD_REG_BLC := Bloco_9.Registro9030Count;
        end;
      end;

    end;

  end;
end;

procedure TACBrDeSTDA.WriteRegistro9900;
begin
  with Bloco_9.Registro9900 do
  begin
     with New do
     begin
        REG_BLC    := '9900';
        QTD_REG_BLC:= Count + 2;
     end;

     with New do
     begin
        REG_BLC    := '9990';
        QTD_REG_BLC:= 1;
     end;

     with New do
     begin
        REG_BLC    := '9999';
        QTD_REG_BLC:= 1;
     end;
  end;
  Bloco_9.WriteRegistro9900;
end;

procedure TACBrDeSTDA.WriteRegistro9990;
begin
  Bloco_9.WriteRegistro9990;
end;

procedure TACBrDeSTDA.WriteRegistro9999;
begin
  Bloco_9.Registro9999.QTD_LIN := Bloco_9.Registro9999.QTD_LIN + Bloco_0.Registro0990.QTD_LIN_0 +
                                                                 Bloco_G.RegistroG990.QTD_LIN_G +
                                                                 Bloco_9.Registro9990.QTD_LIN_9;
  Bloco_9.WriteRegistro9999;
end;

procedure TACBrDeSTDA.WriteRegistroG001;
begin
  // Preenche as classes com os dados
  Bloco_G.WriteRegistroG001;
  //
  with Bloco_9.Registro9900 do
  begin
    with New do
    begin
      REG_BLC := 'G001';
      QTD_REG_BLC := 1;
    end;
  end;
  if Bloco_G.RegistroG001.IND_MOV = imComDados then
  begin
    with Bloco_9.Registro9900 do
    begin
      with New do
      begin
        REG_BLC := 'G020';
        QTD_REG_BLC := 1;
      end;

      with New do
      begin
        REG_BLC := 'G600';
        QTD_REG_BLC := 1;
      end;
      
      if Bloco_G.RegistroG605Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'G605';
          QTD_REG_BLC := Bloco_G.RegistroG605Count;
        end;
      end;

      if Bloco_G.RegistroG610Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'G610';
          QTD_REG_BLC := Bloco_G.RegistroG610Count;
        end;
      end;

      if Bloco_G.RegistroG615Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'G615';
          QTD_REG_BLC := Bloco_G.RegistroG615Count;
        end;
      end;

      if Bloco_G.RegistroG620Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'G620';
          QTD_REG_BLC := Bloco_G.RegistroG620Count;
        end;
      end;

      if Bloco_G.RegistroG625Count > 0 then
      begin
        with New do
        begin
          REG_BLC := 'G625';
          QTD_REG_BLC := Bloco_G.RegistroG625Count;
        end;
      end;
    end;
  end;
end;

procedure TACBrDeSTDA.WriteRegistroG990;
begin
  with Bloco_9.Registro9900.New do
  begin
     REG_BLC    := 'G990';
     QTD_REG_BLC:= 1;
  end;
  Bloco_G.WriteRegistroG990;
end;

{$IFDEF FPC}
initialization
   {$i ACBrDeSTDA.lrs}
{$ENDIF}

end.
