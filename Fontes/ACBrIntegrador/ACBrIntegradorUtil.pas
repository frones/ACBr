unit ACBrIntegradorUtil;

interface

uses
  Classes, SysUtils, pcnGerador, pcnLeitor, pcnConversao, ACBrUtil, dateutils;

type
  { TComandoIntegrador }
  TComandoIntegrador = class(TPersistent)
  private
    FLeitor: TLeitor;
    FPastaInput: String;
    FPastaOutput: String;
    FTimeout: Integer;
    FErroTimeout: Boolean;

  public
    constructor Create;
    destructor Destroy; override;

    function EnviaComando(numeroSessao: Integer; Nome, Comando : String; TimeOutComando : Integer = 0) : String;
    function PegaResposta(Resp : String) : String;
    function AguardaArqResposta(numeroSessao: Integer) : String;
    function AjustaComando(Comando : String) : String;
  published
    property PastaInput  : String  read FPastaInput  write FPastaInput;
    property PastaOutput : String  read FPastaOutput write FPastaOutput;
    property Timeout     : Integer read FTimeout     write FTimeout default 30;
    property ErroTimeout : Boolean read FErroTimeout;
  end;

  { TIdentificador }
  TIdentificador = class(TPersistent)
  private
    FGerador: TGerador;
  public
    constructor Create(AOwner: TGerador);
    destructor Destroy; override;
    procedure GerarIdentificador( Identificador : String );
  end;

  { TMetodo }
  TMetodo = class(TPersistent)
  private
    FGerador: TGerador;
    FIdentificador: TIdentificador;
    FAdicionarParametros : Boolean;
  public
    constructor Create(AOwner: TGerador);
    destructor Destroy; override;
    procedure GerarMetodo( Valor : Integer; Componente, Metodo : String );
    procedure FinalizarMetodo;

    property AdicionarParametros : Boolean read FAdicionarParametros write FAdicionarParametros default True;
  end;

  { TConstrutor }
  TConstrutor = class(TPersistent)
  private
    FGerador: TGerador;
  public
    constructor Create(AOwner: TGerador);
    destructor Destroy; override;
    procedure GerarConstructor( Nome, Valor: String );
  end;

  { TParametro }
  TParametro = class(TPersistent)
  private
    FGerador: TGerador;
  public
    constructor Create(AOwner: TGerador);
    destructor Destroy; override;
    procedure GerarParametro( Nome: String; Valor: Variant; Tipo: TpcnTipoCampo; ParseTextoXML: Boolean = True );
  end;

implementation


{ TComandoIntegrador }

constructor TComandoIntegrador.Create;
begin
  FLeitor        := TLeitor.Create;

  FPastaInput  := 'C:\Integrador\Input\';
  FPastaOutput := 'C:\Integrador\Output\';
  FTimeout     := 30;
  FErroTimeout   := False;
end;

destructor TComandoIntegrador.Destroy;
begin
  FLeitor.Free;
  inherited Destroy;
end;

function TComandoIntegrador.EnviaComando(numeroSessao: Integer; Nome, Comando: String; TimeOutComando : Integer = 0): String;
var
  LocTimeOut, ActualTime, TimeToRetry : TDateTime;
  NomeArquivo : String;
begin
  Result := '';
  FErroTimeout   := False;

  NomeArquivo := PathWithDelim(FPastaInput)+LowerCase(Nome+'-'+IntToStr(numeroSessao)+'.tmp');
  WriteToFile(NomeArquivo,Comando);
  RenameFile(NomeArquivo, ChangeFileExt(NomeArquivo,'.xml'));
  NomeArquivo := ChangeFileExt(NomeArquivo,'.xml');

  ActualTime  := Now;
  TimeToRetry := IncSecond(ActualTime,5);
  if TimeOutComando > 0 then
    FTimeout := TimeOutComando;

  if FTimeout <= 0 then
    LocTimeOut := IncSecond(ActualTime, 30)
  else
    LocTimeOut := IncSecond(ActualTime, FTimeout);

  Result := AguardaArqResposta(numeroSessao);
  while EstaVazio(Result) and
        (ActualTime < LocTimeOut) do
  begin
    Result := AguardaArqResposta(numeroSessao);
    Sleep(50);
    ActualTime := Now;
    if ActualTime > TimeToRetry then //Caso arquivo ainda não tenha sido consumido após 5 segundos, recria o arquivo
    begin
      TimeToRetry := IncSecond(ActualTime,5);
      if FilesExists(NomeArquivo) then
      begin
        try
          DeleteFile(NomeArquivo);
        except
        end;
        NomeArquivo := PathWithDelim(FPastaInput)+LowerCase(Nome+'-'+IntToStr(numeroSessao)+'-'+FormatDateTime('HHNNSS', ActualTime)+'.tmp');
        WriteToFile(NomeArquivo,Comando);
        RenameFile(NomeArquivo, ChangeFileExt(NomeArquivo,'.xml'));
        NomeArquivo := ChangeFileExt(NomeArquivo,'.xml');
      end;
    end;
  end;

  if EstaVazio(Result) then
  begin
    if FilesExists(ChangeFileExt(NomeArquivo,'.xml')) then
      DeleteFile(ChangeFileExt(NomeArquivo,'.xml'));

    FErroTimeout := True;
    raise Exception.Create('Sem Resposta do Integrador');
  end;
end;

function TComandoIntegrador.PegaResposta(Resp: String): String;
begin
  FLeitor.Arquivo := Resp;
  if FLeitor.rExtrai(1, 'Resposta') <> '' then
    Result := FLeitor.rCampo(tcStr, 'retorno')
  else if FLeitor.rExtrai(1, 'Erro') <> '' then
    Result := FLeitor.Grupo
  else
    Result := Resp
end;

function TComandoIntegrador.AguardaArqResposta(numeroSessao: Integer): String;
var
  SL, SLArqResp : TStringList;
  I, J, MaxTentativas : Integer;
  Erro : Boolean;
  Arquivo: String;
begin
  Result := '';
  SL := TStringList.Create;
  SLArqResp := TStringList.Create;
  try
    SLArqResp.Clear;
    FindFiles(PathWithDelim(FPastaOutput)+'*.xml',SLArqResp);
    Sleep(50); //Tentar evitar ler arquivo enquanto está sendo escrito

    for I:=0  to SLArqResp.Count-1 do
    begin
      SL.Clear;

      try
        SL.LoadFromFile(SLArqResp[I]); //ERRO: Unable to open
        Arquivo := SL.Text;
      except
        J := 0;
        MaxTentativas := 5;
        while J < MaxTentativas do
        begin
          try
            Erro := False;
            Sleep(500);
            SL.LoadFromFile(SLArqResp[I]); //ERRO: Unable to open
            Arquivo := SL.Text;
          except
            Erro := True;
            if J = (MaxTentativas-1) then
              Arquivo := ''; //Caso não consigo abrir, retorna vazio
          end;
          if not Erro then
            Break;
          Inc(J);
        end;
      end;

      FLeitor.Arquivo := Arquivo;
      if FLeitor.rExtrai(1, 'Identificador') <> '' then
      begin
        if FLeitor.rCampo(tcInt, 'Valor') = numeroSessao then
        begin
          Result := Trim(FLeitor.Arquivo);
          DeleteFile(SLArqResp[I]);
          Exit;
        end;
      end;
    end;
  finally
    SLArqResp.Free;
    SL.Free;
  end;
end;

function TComandoIntegrador.AjustaComando(Comando: String): String;
begin
  Comando := ChangeLineBreak(Comando,'');

  while pos('  ', Comando) > 0 do
    Comando := StringReplace(Comando, '  ', ' ', [rfReplaceAll]);

  Comando := StringReplace(Comando, '> <', '><', [rfReplaceAll]);;
  //Comando := StringReplace(Comando,'<'+ENCODING_UTF8+'>','',[rfReplaceAll]);
  Result := Comando;
end;

{ TMetodo }

constructor TMetodo.Create(AOwner: TGerador);
begin
  FGerador := AOwner;
  FIdentificador := TIdentificador.Create(FGerador);
  FAdicionarParametros := True;
end;

destructor TMetodo.Destroy;
begin
  FIdentificador.Free;
  inherited Destroy;
end;

procedure TMetodo.GerarMetodo(Valor: Integer; Componente, Metodo: String);
begin
  FGerador.wGrupo('Integrador');
  FIdentificador.GerarIdentificador(IntToStr(Valor));
  FGerador.wGrupo('Componente Nome="'+Componente+'"');
  FGerador.wGrupo('Metodo Nome="'+Metodo+'"');
  if AdicionarParametros then
    FGerador.wGrupo('Parametros');
end;

procedure TMetodo.FinalizarMetodo;
begin
  if AdicionarParametros then
    FGerador.wGrupo('/Parametros');
  FGerador.wGrupo('/Metodo');
  FGerador.wGrupo('/Componente');
  FGerador.wGrupo('/Integrador');
end;

{ TParametro }

constructor TParametro.Create(AOwner: TGerador);
begin
  FGerador := AOwner
end;

destructor TParametro.Destroy;
begin
  inherited Destroy;
end;

procedure TParametro.GerarParametro(Nome: String; Valor: Variant;
  Tipo: TpcnTipoCampo; ParseTextoXML: Boolean);
begin
  FGerador.wGrupo('Parametro');
  FGerador.wCampo(tcStr, '', 'Nome', 1, 99, 1, Nome, 'Nome do Parâmetro');
  FGerador.wCampo(Tipo , '', 'Valor', 1, 99, 1, Valor, 'Valor do Parâmetro',ParseTextoXML);
  FGerador.wGrupo('/Parametro');
end;

{ TConstrutor }

constructor TConstrutor.Create(AOwner: TGerador);
begin
   FGerador := AOwner
end;

destructor TConstrutor.Destroy;
begin
  inherited Destroy;
end;

procedure TConstrutor.GerarConstructor(Nome, Valor: String);
begin
  FGerador.wGrupo('Construtor');
  FGerador.wGrupo('Parametros');
  FGerador.wGrupo('Parametro');
  FGerador.wCampo(tcStr, '', 'Nome', 1, 99, 1, Nome, 'Nome do Construtor');
  FGerador.wCampo(tcStr, '', 'Valor', 1, 99, 1, Valor, 'Valor do Construtor');
  FGerador.wGrupo('/Parametro');
  FGerador.wGrupo('/Parametros');
  FGerador.wGrupo('/Construtor');
end;

{ TIdentificador }

constructor TIdentificador.Create(AOwner: TGerador);
begin
  FGerador := AOwner;
end;

destructor TIdentificador.Destroy;
begin
  inherited Destroy;
end;

procedure TIdentificador.GerarIdentificador(Identificador: String);
begin
  FGerador.wGrupo('Identificador');
  FGerador.wCampo(tcStr, '', 'Valor', 1, 99, 1, Identificador, 'Valor do Identificador');
  FGerador.wGrupo('/Identificador');
end;

end.

