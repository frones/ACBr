unit pcnMFeUtil;

interface

uses
  Classes, SysUtils, pcnGerador, pcnLeitor, pcnConversao, ACBrUtil, dateutils;

type
  { TComandoMFe }
  TComandoMFe = class(TPersistent)
  private
    FLeitor: TLeitor;
    FPastaInput : String;
    FPastaOutput : String;
    FTimeout : Integer;

  public
    constructor Create;
    destructor Destroy; override;

    function EnviaComando(numeroSessao: Integer; Comando : String) : String;
    function PegaResposta(Resp : String) : String;
    function AguardaArqResposta(numeroSessao: Integer) : String;
    function AjustaComando(Comando : String) : String;
  published
    property PastaInput  : String  read FPastaInput  write FPastaInput;
    property PastaOutput : String  read FPastaOutput write FPastaOutput;
    property Timeout     : Integer read FTimeout     write FTimeout default 30;

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


{ TComandoMFe }

constructor TComandoMFe.Create;
begin
  FLeitor        := TLeitor.Create;

  FPastaInput  := 'C:\Integrador\Input\';
  FPastaOutput := 'C:\Integrador\Output\';
  FTimeout     := 30;
end;

destructor TComandoMFe.Destroy;
begin
  FLeitor.Free;
  inherited Destroy;
end;

function TComandoMFe.EnviaComando(numeroSessao: Integer; Comando: String): String;
var
  SL : TStringList;
  LocTimeOut, ActualTime : TDateTime;
begin
  SL := TStringList.Create;
  try
    SL.Add(Comando);
    SL.SaveToFile(PathWithDelim(FPastaInput)+'Comando.tmp'); // Para evitar a leitura pelo integrador antes do arquivo estar completamente gravado.
    RenameFile(PathWithDelim(FPastaInput)+'Comando.tmp', PathWithDelim(FPastaInput)+'Comando.xml');

    ActualTime := Now;
    if FTimeout <= 0 then
      LocTimeOut := IncSecond(ActualTime, 30)
    else
      LocTimeOut := IncSecond(ActualTime, FTimeout);
    Result := AguardaArqResposta(numeroSessao);
    while EstaVazio(Result) and
          (ActualTime < LocTimeOut) do
    begin
      Result := AguardaArqResposta(numeroSessao);
      Sleep(100);
      ActualTime := Now;
    end;
  finally
    SL.Free;
  end;
end;

function TComandoMFe.PegaResposta(Resp: String): String;
begin
  FLeitor.Arquivo := Resp;
  if FLeitor.rExtrai(1, 'Resposta') <> '' then
    Result := FLeitor.rCampo(tcStr, 'retorno')
  else if FLeitor.rExtrai(1, 'Erro') <> '' then
    Result := FLeitor.Grupo;
end;

function TComandoMFe.AguardaArqResposta(numeroSessao: Integer): String;
var
  SL, SLArqResp : TStringList;
  I : Integer;
begin
  SL := TStringList.Create;
  SLArqResp := TStringList.Create;
  try
    SLArqResp.Clear;
    FindFiles(PathWithDelim(FPastaOutput)+'*.xml',SLArqResp);
    Sleep(100); //Tentar evitar ler arquivo enquanto está sendo escrito

    for I:=0  to SLArqResp.Count-1 do
    begin
      SL.Clear;
      SL.LoadFromFile(SLArqResp[I]);
      FLeitor.Arquivo := SL.Text;
      if FLeitor.rExtrai(1, 'Identificador') <> '' then
      begin
        if FLeitor.rCampo(tcInt, 'Valor') = numeroSessao then
        begin
          Result := Trim(FLeitor.Arquivo);
          DeleteFile(SLArqResp[I]);
        end;
      end;
    end;
  finally
    SLArqResp.Free;
    SL.Free;
  end;
end;

function TComandoMFe.AjustaComando(Comando: String): String;
begin
  Comando := ChangeLineBreak(Comando,'');

  while pos('  ', Comando) > 0 do
    Comando := StringReplace(Comando, '  ', ' ', [rfReplaceAll]);

  Comando := StringReplace(Comando, '> <', '><', [rfReplaceAll]);;
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
  FGerador.wCampo(tcStr, '', 'Valor', 1, 99, 1, Valor, 'Valor do Parâmetro',ParseTextoXML);
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

