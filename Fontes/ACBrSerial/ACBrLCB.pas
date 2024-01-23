{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:    Marcelo Welter - www.welter.pro.br/sultan    }
{                                 Alexandre Rocha Lima e Marcondes             }
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

unit ACBrLCB;

interface
uses ACBrBase, ACBrDevice  {Units da ACBr}
     {$IFNDEF NOGUI}
       {$IF DEFINED(VisualCLX)}
          ,QExtCtrls
       {$ELSEIF DEFINED(FMX)}
          ,FMX.ExtCtrls, FMX.Types
       {$ELSE}
          ,ExtCtrls
       {$IFEND}
     {$ENDIF}
     ,SysUtils, Classes;

type

{ Componente ACBrLCB - Leitor de Código de Barras }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrLCB = class( TACBrComponent )
  private
    fsDevice  : TACBrDevice ;   { SubComponente ACBrDevice }
    {$IFNDEF NOGUI}
      fsTimer: TTimer;
    {$ELSE}
      fsTimer: TACBrThreadTimer;
    {$ENDIF}

    { Propriedades do Componente ACBrLCB }
    fsAtivo  : Boolean;
    fsOnLeCodigo: TNotifyEvent;
    fsOnLeFila : TNotifyEvent;
    fsUsarFila: Boolean;
    fsFilaMaxItens: Integer;
    fsExcluirSufixo: Boolean;
    fsSufixo: AnsiString;
    fs_Sufixo: AnsiString;
    fsPrefixoAExcluir: AnsiString;
    fs_PrefixoAExcluir: AnsiString;
    fsUltimaLeitura: AnsiString;
    fsUltimoCodigo: AnsiString;
    fsIntervalo: Integer;
    fsPrefixoAIncluir: AnsiString;

    procedure SetPorta(const Value: String);
    procedure SetAtivo(const Value: Boolean);

    function GetPorta: String;
    function GetFilaCount: Integer;

    procedure LeSerial(Sender: TObject);
    procedure SetUltimoCodigo(const Value: AnsiString);
    procedure SetPrefixoAExcluir(const Value: AnsiString);
    procedure SetSufixo(const Value: AnsiString);
    procedure AddFila( const AString : AnsiString ) ;
    procedure SetUsarFila(const Value: Boolean);
    procedure SetIntervalo(const Value: Integer);
  protected

  public
    Fila : TStringList ;

    constructor Create(AOwner: TComponent); override;
    Destructor Destroy  ; override ;

    procedure Ativar ;
    procedure Desativar ;
    property Ativo : Boolean read fsAtivo write SetAtivo ;

    function LerFila : AnsiString ;
    procedure ApagarFila ;
    property FilaCount : Integer read GetFilaCount ;

    procedure EnviarString( const AString : AnsiString ) ;
    function LerString : AnsiString ;
     
    property UltimaLeitura : AnsiString read fsUltimaLeitura ;
    property UltimoCodigo  : AnsiString read fsUltimoCodigo ;
  published
     { Instancia do Componente ACBrDevice }
     property Device : TACBrDevice         read fsDevice ;
     property Porta : String               read GetPorta          write SetPorta ;
     property PrefixoAIncluir : AnsiString read fsPrefixoAIncluir write fsPrefixoAIncluir ;
     property PrefixoAExcluir : AnsiString read fsPrefixoAExcluir write SetPrefixoAExcluir;
     property Sufixo : AnsiString          read fsSufixo          write SetSufixo ;
     property ExcluirSufixo : Boolean      read fsExcluirSufixo   write fsExcluirSufixo    default true ;
     property UsarFila : Boolean           read fsUsarFila        write SetUsarFila        default false ;
     property FilaMaxItens : Integer       read fsFilaMaxItens    write fsFilaMaxItens     default 0 ;
     property Intervalo  : Integer         read fsIntervalo       write SetIntervalo       default 200 ;
     property OnLeCodigo : TNotifyEvent    read fsOnLeCodigo      write fsOnLeCodigo ;
     property OnLeFila   : TNotifyEvent    read fsOnLeFila        write fsOnLeFila   ;
end ;

implementation

uses
    {$IFDEF COMPILER6_UP} DateUtils, StrUtils, {$ELSE} ACBrD5, {$ENDIF}
     ACBrUtil.Strings,
     ACBrUtil.Base;

{ TACBrLCB }

constructor TACBrLCB.Create(AOwner: TComponent);
begin
  inherited create( AOwner );

  fsAtivo           := false ;
  fsPrefixoAIncluir := '' ;
  fsPrefixoAExcluir := '' ;
  fs_PrefixoAExcluir:= '' ;
  fsSufixo          := '' ;
  fs_Sufixo         := '' ;
  fsExcluirSufixo   := true ;
  fsUsarFila        := false ;
  fsFilaMaxItens    := 0 ;
  fsUltimaLeitura   := '' ;
  fsOnLeCodigo      := nil ;
  fsOnLeFila        := nil ;
  fsIntervalo       := 300 ;

  Fila := TStringList.Create ;

  { Instanciando SubComponente TACBrDevice }
  fsDevice := TACBrDevice.Create( self ) ;  { O dono é o proprio componente }
  fsDevice.Name := 'ACBrDevice' ;      { Apenas para aparecer no Object Inspector}
  {$IFDEF COMPILER6_UP}
  fsDevice.SetSubComponent( true );{ para gravar no DFM/XFM }
  {$ENDIF}
  fsDevice.Porta := 'COM1';
  fsDevice.TimeOut := 1 ;

  { Timer para monitorar a Porta Serial }
  {$IFNDEF NOGUI}
    fsTimer := TTimer.Create(self) ;
  {$ELSE}
    fsTimer := TACBrThreadTimer.Create ;   
  {$ENDIF}

  fsTimer.Enabled := False ;
  fsTimer.OnTimer := LeSerial ;
end;

destructor TACBrLCB.Destroy;
begin
  Desativar ;

  fsTimer.Enabled := False ;
  fsTimer.Free ;

  FreeAndNil( fsDevice ) ;
  FreeAndNil( Fila );

  inherited Destroy;
end;

procedure TACBrLCB.SetAtivo(const Value: Boolean);
begin
  if Value then
     Ativar
  else
     Desativar ;
end;

procedure TACBrLCB.Ativar;
begin
  if fsAtivo then exit ;

  if (copy(UpperCase(Porta),1,7) <> 'TECLADO') and (not fsDevice.IsSerialPort)  then
     raise Exception.Create(ACBrStr('O ACBrLCB foi projetado para Leitores de'+#10+
                            ' Código de Barras ligados a Portas Seriais.'));

  try
     fsDevice.Ativar ;

     if fsIntervalo = 0 then
        fsIntervalo := 200 ;

     fsTimer.Interval := fsIntervalo ;
     fsTimer.Enabled  := true ;

     fsAtivo := true ;
  except
     Desativar ;
     raise ;
  end ;
end;

procedure TACBrLCB.Desativar;
begin
  if not fsAtivo then exit ;

  fsDevice.Desativar ;
  fsTimer.Enabled := false ; 

  fsAtivo := false;
end;

function TACBrLCB.GetPorta: String;
begin
  result := fsDevice.Porta ;
end;

procedure TACBrLCB.SetPorta(const Value: String);
begin
  fsDevice.Porta := Value ;
end;

function TACBrLCB.GetFilaCount: Integer;
begin
  Result := Fila.Count ;
end;

procedure TACBrLCB.ApagarFila;
begin
  Fila.Clear ;
end;

function TACBrLCB.LerFila: AnsiString;
begin
  Result := '' ;
  if Fila.Count > 0 then
  begin
     Result := Fila[0] ;
     Fila.Delete( 0 ) ;

     SetUltimoCodigo( Result );
     Result := fsUltimoCodigo ;
     if Assigned( fsOnLeFila ) then
        fsOnLeFila( self ) ;
  end
end;

procedure TACBrLCB.SetUltimoCodigo(const Value: AnsiString);
begin
  { acertando codigo, removendo prefixo, sufixo }
  fsUltimoCodigo := Value;
  if fs_PrefixoAExcluir <> '' then
     if LeftStr(fsUltimoCodigo,Length(fs_PrefixoAExcluir)) =
        fs_PrefixoAExcluir then
        fsUltimoCodigo := copy(fsUltimoCodigo, Length(fs_PrefixoAExcluir)+1,
                                               Length(fsUltimoCodigo) ) ;

  if fsExcluirSufixo and (fs_Sufixo <> '') then
     if RightStr(fsUltimoCodigo,Length(fs_Sufixo)) = fs_Sufixo then
        fsUltimoCodigo := copy(fsUltimoCodigo, 1,
                          Length(fsUltimoCodigo)-Length(fs_Sufixo) ) ;
end;

procedure TACBrLCB.SetPrefixoAExcluir(const Value: AnsiString);
begin
  fsPrefixoAExcluir  := Value;
  fs_PrefixoAExcluir := TraduzComando( Value ) ;
end;

procedure TACBrLCB.SetSufixo(const Value: AnsiString);
begin
  if Value = '' then  {Isso irá forçar o Sufixo ser gravado no DFM/XFM mesmo que}
     fsSufixo := ' '  {ele seja vazio. Necessário, pois ele é criado com #13 }
  else
     fsSufixo := Value;
     
  fs_Sufixo := TraduzComando( Value ) ;
end;

procedure TACBrLCB.AddFila(const AString: AnsiString);
begin
  if Fila.Count = FilaMaxItens then
     Fila.Delete( 0 ) ;

  Fila.Add( AString ) ;
end;

procedure TACBrLCB.SetUsarFila(const Value: Boolean);
begin
  if Value then
   begin
     if Sufixo = '' then  
        raise Exception.Create( ACBrStr('É necessário definir um Sufixo '+sLineBreak+
                                'para manipular Filas... Ex: #13' )) ;

     if FilaMaxItens = 0 then
        FilaMaxItens := 100 ;
   end
  else
     Fila.Clear ;

  fsUsarFila := Value;
end;

procedure TACBrLCB.LeSerial(Sender: TObject);
var leitura : String ;
    OldRaiseExcept : Boolean ;
begin
  with fsDevice.Serial do
  begin
     if InstanceActive then
     begin
        if WaitingDataEx > 0 then
        begin
           leitura := '' ;
           { Se tem Sufixo definido deve sempre ler um código completo, até
             encontrar o Sufixo correto, caso contrário não lê nada, pois é
             provavel que todos os bytes ainda não chegaram do leitor, nesse
             caso o código completo será lido no proximo evento do Timer.

             Entretanto se o sufixo estiver ERRADO, ou seja, diferente do
             enviado pelo leitor, ele NUNCA lerá NADA... Para TESTES, deixe
             o Sufixo VAZIO... pois nesse caso, TUDO que estiver disponível na
             porta serial será lido e retornado  }

           if fs_Sufixo <> '' then
            begin
              OldRaiseExcept := RaiseExcept ;
              try
                 { Desliga a RaiseExcept para nao perder o Buffer da serial, se
                   RecvTerminated falhar (bug da Synaser) }
                 RaiseExcept := false ;
                 leitura     := RecvTerminated(200,fs_Sufixo) ;
                 if leitura <> '' then
                    leitura := leitura + fs_Sufixo ;
              finally
                 RaiseExcept := OldRaiseExcept ;
              end ;
            end
           else
              leitura := RecvPacket(200) ;

           if leitura <> '' then
           begin
              fsUltimaLeitura := leitura ; {Leitura exatamente como veio do Leitor}

              { Quando adicionado um Prefixo adiciona valor para o código lido e libera o Prefixo indicado }
              {assim possibilitando controle de inclusão de multiplicadores para operação de leitura}

              if (fsPrefixoAIncluir <> '') then
              begin
                 leitura           :=  fsPrefixoAIncluir + leitura;
                 fsPrefixoAIncluir := '';
              end;

              if UsarFila then
                 AddFila( leitura ) ;

              SetUltimoCodigo( leitura );
              if Assigned( fsOnLeCodigo ) then
                 fsOnLeCodigo( self ) ;
           end ;
        end;
     end ;
  end ;
end;

procedure TACBrLCB.SetIntervalo(const Value: Integer);
begin
  fsTimer.Interval := Value ;
  fsIntervalo      := fsTimer.Interval ;
  fsTimer.Enabled  := fsAtivo and (fsIntervalo > 0) ;
end;

procedure TACBrLCB.EnviarString(const AString: AnsiString);
begin
  if not fsAtivo then
     raise Exception.Create(ACBrStr('Componente ACBrLCB não está ATIVO'));

  fsDevice.Limpar ;
  fsDevice.EnviaString( AString );
end;

function TACBrLCB.LerString: AnsiString;
begin
  if not fsAtivo then
     raise Exception.Create(ACBrStr('Componente ACBrLCB não está ATIVO'));

  Result := fsDevice.LeString(200) ;
end;

end.
