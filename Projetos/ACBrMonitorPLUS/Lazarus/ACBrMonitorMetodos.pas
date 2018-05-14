{******************************************************************************}
{ Projeto: ACBr Monitor                                                        }
{  Executavel multiplataforma que faz usocdo conjunto de componentes ACBr para }
{ criar uma interface de comunicação com equipamentos de automacao comercial.  }
{                                                                              }
{ Direitos Autorais Reservados (c) 2010 Daniel Simões de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:     2005 Fábio Rogério Baía                     }
{                                                                              }
{  Você pode obter a última versão desse arquivo na página do Projeto ACBr     }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{  Este programa é software livre; você pode redistribuí-lo e/ou modificá-lo   }
{ sob os termos da Licença Pública Geral GNU, conforme publicada pela Free     }
{ Software Foundation; tanto a versão 2 da Licença como (a seu critério)       }
{ qualquer versão mais nova.                                                   }
{                                                                              }
{  Este programa é distribuído na expectativa de ser útil, mas SEM NENHUMA     }
{ GARANTIA; nem mesmo a garantia implícita de COMERCIALIZAÇÃO OU DE ADEQUAÇÃO A}
{ QUALQUER PROPÓSITO EM PARTICULAR. Consulte a Licença Pública Geral GNU para  }
{ obter mais detalhes. (Arquivo LICENCA.TXT ou LICENSE.TXT)                    }
{                                                                              }
{  Você deve ter recebido uma cópia da Licença Pública Geral GNU junto com este}
{ programa; se não, escreva para a Free Software Foundation, Inc., 59 Temple   }
{ Place, Suite 330, Boston, MA 02111-1307, USA. Você também pode obter uma     }
{ copia da licença em:  http://www.opensource.org/licenses/gpl-license.php     }
{                                                                              }
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{       Rua Coronel Aureliano de Camargo, 973 - Tatuí - SP - 18270-170         }
{                                                                              }
{******************************************************************************}

unit ACBrMonitorMetodos;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ACBrUtil, ACBrDFe, ACBrMDFe, ACBrCTe, ACBrNFe, ACBrGNRE2,
  ACBrMonitorConsts;

type

{ TDFeMetodo }

  TDFeMetodo = class
  private
    FXMLorFile           : String;
    FPathDFe             : String;
    FPathDFeExtensao     : String;

    procedure CarregarDFePath( const AValue: String ); virtual;
    procedure CarregarDFeXML( const AValue: String ); virtual;
    function ValidarDFe( const AValue: String ): Boolean; virtual;

    function ValidarPath(const AValue: String): Boolean;
    procedure SetFXMLorFile(const AValue: String);

  protected
    FpACBrDFe: TACBrDFe;
    FMsgValidaPath : TStringList;

    property XMLorFile : String read FXMLorFile write SetFXMLorFile;

  public
    constructor Create(AACBrDFe: TACBrDFe; AXMLorFile: String );
    Destructor  Destroy; Override;

    property PathDFe        : String read FPathDFe;
    property PathDFeExtensao: String read FPathDFeExtensao;

  end;

  { TDFeMetodoMDFe }

  TDFeMetodoMDFe = class(TDFeMetodo)
  private
    procedure CarregarDFePath( const AValue: String ); override;
    procedure CarregarDFeXML( const AValue: String ); override;
    function ValidarDFe( const AValue: String ): Boolean; override;

  public
    constructor Create(AACBrDFe: TACBrMDFe; AXMLorFile: String ); reintroduce;
    Destructor  Destroy; Override;

  end;

  { TDFeMetodoMDFeEvento }

  TDFeMetodoMDFeEvento = class(TDFeMetodo)
  private
    procedure CarregarDFePath( const AValue: String ); override;
    procedure CarregarDFeXML( const AValue: String ); override;
    function ValidarDFe( const AValue: String ): Boolean; override;

  public
    constructor Create(AACBrDFe: TACBrMDFe; AXMLorFile: String ); reintroduce;
    Destructor  Destroy; Override;

  end;

  { TDFeMetodoCTe }

  TDFeMetodoCTe = class(TDFeMetodo)
  private
    procedure CarregarDFePath( const AValue: String ); override;
    procedure CarregarDFeXML( const AValue: String ); override;
    function ValidarDFe( const AValue: String ): Boolean; override;

  public
    constructor Create(AACBrDFe: TACBrCTe; AXMLorFile: String ); reintroduce;
    Destructor  Destroy; Override;

  end;

  { TDFeMetodoCTeEvento }

  TDFeMetodoCTeEvento = class(TDFeMetodo)
  private
    procedure CarregarDFePath( const AValue: String ); override;
    procedure CarregarDFeXML( const AValue: String ); override;
    function ValidarDFe( const AValue: String ): Boolean; override;

  public
    constructor Create(AACBrDFe: TACBrCTe; AXMLorFile: String ); reintroduce;
    Destructor  Destroy; Override;

  end;

  { TDFeMetodoCTeInutilizacao }

  TDFeMetodoCTeInutilizacao = class(TDFeMetodo)
  private
    procedure CarregarDFePath( const AValue: String ); override;
    procedure CarregarDFeXML( const AValue: String ); override;
    function ValidarDFe( const AValue: String ): Boolean; override;

  public
    constructor Create(AACBrDFe: TACBrCTe; AXMLorFile: String ); reintroduce;
    Destructor  Destroy; Override;

  end;

  { TDFeMetodoNFe }

  TDFeMetodoNFe = class(TDFeMetodo)
  private
    procedure CarregarDFePath( const AValue: String ); override;
    procedure CarregarDFeXML( const AValue: String ); override;
    function ValidarDFe( const AValue: String ): Boolean; override;

  public
    constructor Create(AACBrDFe: TACBrNFe; AXMLorFile: String ); reintroduce;
    Destructor  Destroy; Override;

  end;

  { TDFeMetodoNFeEvento }

  TDFeMetodoNFeEvento = class(TDFeMetodo)
  private
    procedure CarregarDFePath( const AValue: String ); override;
    procedure CarregarDFeXML( const AValue: String ); override;
    function ValidarDFe( const AValue: String ): Boolean; override;

  public
    constructor Create(AACBrDFe: TACBrNFe; AXMLorFile: String ); reintroduce;
    Destructor  Destroy; Override;

  end;

  { TDFeMetodoNFeInutilizacao }

  TDFeMetodoNFeInutilizacao = class(TDFeMetodo)
  private
    procedure CarregarDFePath( const AValue: String ); override;
    procedure CarregarDFeXML( const AValue: String ); override;
    function ValidarDFe( const AValue: String ): Boolean; override;

  public
    constructor Create(AACBrDFe: TACBrNFe; AXMLorFile: String ); reintroduce;
    Destructor  Destroy; Override;

  end;

  { TDFeMetodoGNRe }

  TDFeMetodoGNRe = class(TDFeMetodo)
  private
    procedure CarregarDFePath( const AValue: String ); override;
    procedure CarregarDFeXML( const AValue: String ); override;
    function ValidarDFe( const AValue: String ): Boolean; override;

  public
    constructor Create(AACBrDFe: TACBrGNRE; AXMLorFile: String ); reintroduce;
    Destructor  Destroy; Override;

  end;


implementation

uses
  DoACBrUnit;

{ TDFeMetodo }

procedure TDFeMetodo.CarregarDFePath(const AValue: String);
begin
  raise Exception.Create(SErroNaoImplementado);  //Método Virtual
end;

procedure TDFeMetodo.CarregarDFeXML(const AValue: String);
begin
  raise Exception.Create(SErroNaoImplementado);  //Método Virtual
end;

function TDFeMetodo.ValidarDFe( const AValue: String ): Boolean;
begin
  Result := False;
  raise Exception.Create(SErroNaoImplementado);   //Método Virtual
end;

function TDFeMetodo.ValidarPath(const AValue: String): Boolean;
begin
  Result := False;
  if (FilesExists( AValue ) ) then
    Result:= True
  else
    FMsgValidaPath.Add( Format( SErroArqNaoEncontado, [AValue] ) );
end;

procedure TDFeMetodo.SetFXMLorFile(const AValue: String);
var
  IsXML : Boolean;
begin
  try
    IsXML := StringIsXML(AValue);

    if not(IsXML) then
    begin
      try
        if ValidarPath(AValue) then
          CarregarDFePath(AValue);

        if not(ValidarDFe(AValue) ) then
        begin
          if ValidarPath(FPathDFe) then
            CarregarDFePath(FPathDFe);
        end;

        if not(ValidarDFe(AValue) ) then
        begin
          if ValidarPath(FPathDFeExtensao) then
            CarregarDFePath(FPathDFeExtensao)
          else
            raise Exception.Create( ACBrStr( FMsgValidaPath.Text ) );
        end;

      finally
        FMsgValidaPath.Clear;
      end;
    end
    else
      CarregarDFeXML(AValue);

  except
    on E: Exception do
    begin
      raise Exception.Create(E.Message);
    end;
  end;
end;

constructor TDFeMetodo.Create(AACBrDFe: TACBrDFe; AXMLorFile: String);
begin
  inherited Create;
  FMsgValidaPath   := TStringList.Create;
  FPathDFe         := '';
  FPathDFeExtensao := '';
  FpACBrDFe := AACBrDFe;
  XMLorFile := AXMLorFile;

end;

destructor TDFeMetodo.Destroy;
begin
  inherited;
  FMsgValidaPath.Free;
end;

{ TDFeMetodoGNRe }

procedure TDFeMetodoGNRe.CarregarDFePath( const AValue: String);
begin
  if not ( TACBrGNRe(FpACBrDFe).Guias.LoadFromFile( AValue ) ) then
    raise Exception.Create(ACBrStr( Format(SErroGNReAbrir, [AValue]) ) );
end;

procedure TDFeMetodoGNRe.CarregarDFeXML(const AValue: String);
begin
  if not ( TACBrGNre(FpACBrDFe).Guias.LoadFromString( AValue ) ) then
    raise Exception.Create(ACBrStr(SErroGNReCarregar) );
end;

function TDFeMetodoGNRe.ValidarDFe( const AValue: String ): Boolean;
begin
  Result := False;
  if ( TACBrGNRe(FpACBrDFe).Guias.Count > 0 ) then
    Result:= True;

  if EstaVazio( FPathDFe )then
    FPathDFe := PathWithDelim(TACBrGNRe(FpACBrDFe).Configuracoes.Arquivos.PathSalvar)
                + AValue;

  if EstaVazio( FPathDFeExtensao )then
    FPathDFeExtensao := PathWithDelim(TACBrGNRe(FpACBrDFe).Configuracoes.Arquivos.PathSalvar)
                        + AValue + CExtensaoXmlGNRe ;
end;

constructor TDFeMetodoGNRe.Create(AACBrDFe: TACBrGNRe; AXMLorFile: String);
begin
  inherited Create(AACBrDFe, AXMLorFile);
end;

destructor TDFeMetodoGNRe.Destroy;
begin
  inherited;
end;

{ TDFeMetodoNFeInutilizacao }

procedure TDFeMetodoNFeInutilizacao.CarregarDFePath( const AValue: String);
begin
  if not ( TACBrNFe(FpACBrDFe).InutNFe.LerXML( AValue ) ) then
    raise Exception.Create(ACBrStr( Format(SErroNFeAbrir, [AValue]) ) );
end;

procedure TDFeMetodoNFeInutilizacao.CarregarDFeXML(const AValue: String);
begin
  if not ( TACBrNFe(FpACBrDFe).InutNFe.LerXMLFromString( AValue ) ) then
    raise Exception.Create(ACBrStr(SErroNFeCarregar) );
end;

function TDFeMetodoNFeInutilizacao.ValidarDFe( const AValue: String ): Boolean;
begin
  Result := False;
  if Assigned( TACBrNFe(FpACBrDFe).InutNFe ) then
    Result:= True;

  if EstaVazio( FPathDFe )then
    FPathDFe := PathWithDelim(TACBrNFe(FpACBrDFe).Configuracoes.Arquivos.PathSalvar)
                + AValue;

  if EstaVazio( FPathDFeExtensao )then
    FPathDFeExtensao := PathWithDelim(TACBrNFe(FpACBrDFe).Configuracoes.Arquivos.PathSalvar)
                        + AValue + CExtensaoXmlNFeInu ;
end;

constructor TDFeMetodoNFeInutilizacao.Create(AACBrDFe: TACBrNFe; AXMLorFile: String);
begin
  inherited Create(AACBrDFe, AXMLorFile);
end;

destructor TDFeMetodoNFeInutilizacao.Destroy;
begin
  inherited;
end;

{ TDFeMetodoNFeEvento }

procedure TDFeMetodoNFeEvento.CarregarDFePath( const AValue: String);
begin
  if not ( TACBrNFe(FpACBrDFe).EventoNFe.LerXML( AValue ) ) then
    raise Exception.Create(ACBrStr( Format(SErroNFeAbrir, [AValue]) ) );
end;

procedure TDFeMetodoNFeEvento.CarregarDFeXML(const AValue: String);
begin
  if not ( TACBrNFe(FpACBrDFe).EventoNFe.LerXMLFromString( AValue ) ) then
    raise Exception.Create(ACBrStr(SErroNFeCarregar) );
end;

function TDFeMetodoNFeEvento.ValidarDFe( const AValue: String ): Boolean;
begin
  Result := False;
  if ( TACBrNFe(FpACBrDFe).EventoNFe.Evento.Count > 0 ) then
    Result:= True;

  if EstaVazio( FPathDFe )then
    FPathDFe := PathWithDelim(TACBrNFe(FpACBrDFe).Configuracoes.Arquivos.PathSalvar)
                + AValue;

  if EstaVazio( FPathDFeExtensao )then
    FPathDFeExtensao := PathWithDelim(TACBrNFe(FpACBrDFe).Configuracoes.Arquivos.PathSalvar)
                        + AValue + CExtensaoXmlNFeEve ;
end;

constructor TDFeMetodoNFeEvento.Create(AACBrDFe: TACBrNFe; AXMLorFile: String);
begin
  inherited Create(AACBrDFe, AXMLorFile);
end;

destructor TDFeMetodoNFeEvento.Destroy;
begin
  inherited;
end;

{ TDFeMetodoNFe }

procedure TDFeMetodoNFe.CarregarDFePath( const AValue: String);
begin
  if not ( TACBrNFe(FpACBrDFe).NotasFiscais.LoadFromFile( AValue ) ) then
    raise Exception.Create(ACBrStr( Format(SErroNFeAbrir, [AValue]) ) );
end;

procedure TDFeMetodoNFe.CarregarDFeXML(const AValue: String);
begin
  if not ( TACBrNFe(FpACBrDFe).NotasFiscais.LoadFromString( AValue ) ) then
    raise Exception.Create(ACBrStr(SErroNFeCarregar) );
end;

function TDFeMetodoNFe.ValidarDFe( const AValue: String ): Boolean;
begin
  Result := False;
  if ( TACBrNFe(FpACBrDFe).NotasFiscais.Count > 0 ) then
    Result:= True;

  if EstaVazio( FPathDFe )then
    FPathDFe := PathWithDelim(TACBrNFe(FpACBrDFe).Configuracoes.Arquivos.PathSalvar)
                + AValue;

  if EstaVazio( FPathDFeExtensao )then
    FPathDFeExtensao := PathWithDelim(TACBrNFe(FpACBrDFe).Configuracoes.Arquivos.PathSalvar)
                        + AValue + CExtensaoXmlNFe ;
end;

constructor TDFeMetodoNFe.Create(AACBrDFe: TACBrNFe; AXMLorFile: String);
begin
  inherited Create(AACBrDFe, AXMLorFile);
end;

destructor TDFeMetodoNFe.Destroy;
begin
  inherited;
end;

{ TDFeMetodoCTeInutilizacao }

procedure TDFeMetodoCTeInutilizacao.CarregarDFePath( const AValue: String);
begin
  if not ( TACBrCTe(FpACBrDFe).InutCTe.LerXML( AValue ) ) then
    raise Exception.Create(ACBrStr( Format(SErroCTeAbrir, [AValue]) ) );
end;

procedure TDFeMetodoCTeInutilizacao.CarregarDFeXML(const AValue: String);
begin
  if not ( TACBrCTe(FpACBrDFe).InutCTe.LerXMLFromString( AValue ) ) then
    raise Exception.Create(ACBrStr(SErroCTeCarregar) );
end;

function TDFeMetodoCTeInutilizacao.ValidarDFe( const AValue: String ): Boolean;
begin
  Result := False;
  if Assigned( TACBrCTe(FpACBrDFe).InutCTe ) then
    Result:= True;

  if EstaVazio( FPathDFe )then
    FPathDFe := PathWithDelim(TACBrCTe(FpACBrDFe).Configuracoes.Arquivos.PathSalvar)
                + AValue;

  if EstaVazio( FPathDFeExtensao )then
    FPathDFeExtensao := PathWithDelim(TACBrCTe(FpACBrDFe).Configuracoes.Arquivos.PathSalvar)
                        + AValue + CExtensaoXmlCTeInu ;
end;

constructor TDFeMetodoCTeInutilizacao.Create(AACBrDFe: TACBrCTe; AXMLorFile: String);
begin
  inherited Create(AACBrDFe, AXMLorFile);
end;

destructor TDFeMetodoCTeInutilizacao.Destroy;
begin
  inherited;
end;

{ TDFeMetodoCTeEvento }

procedure TDFeMetodoCTeEvento.CarregarDFePath( const AValue: String);
begin
  if not ( TACBrCTe(FpACBrDFe).EventoCTe.LerXML( AValue ) ) then
    raise Exception.Create(ACBrStr( Format(SErroCTeAbrir, [AValue]) ) );
end;

procedure TDFeMetodoCTeEvento.CarregarDFeXML(const AValue: String);
begin
  if not ( TACBrCTe(FpACBrDFe).EventoCTe.LerXMLFromString( AValue ) ) then
    raise Exception.Create(ACBrStr(SErroCTeCarregar) );
end;

function TDFeMetodoCTeEvento.ValidarDFe( const AValue: String ): Boolean;
begin
  Result := False;
  if ( TACBrCTe(FpACBrDFe).EventoCTe.Evento.Count > 0 ) then
    Result:= True;

  if EstaVazio( FPathDFe )then
    FPathDFe := PathWithDelim(TACBrCTe(FpACBrDFe).Configuracoes.Arquivos.PathSalvar)
                + AValue;

  if EstaVazio( FPathDFeExtensao )then
    FPathDFeExtensao := PathWithDelim(TACBrCTe(FpACBrDFe).Configuracoes.Arquivos.PathSalvar)
                        + AValue + CExtensaoXmlCTeEve ;
end;

constructor TDFeMetodoCTeEvento.Create(AACBrDFe: TACBrCTe; AXMLorFile: String);
begin
  inherited Create(AACBrDFe, AXMLorFile);
end;

destructor TDFeMetodoCTeEvento.Destroy;
begin
  inherited;
end;

{ TDFeMetodoCTe }

procedure TDFeMetodoCTe.CarregarDFePath( const AValue: String);
begin
  if not ( TACBrCTe(FpACBrDFe).Conhecimentos.LoadFromFile( AValue ) ) then
    raise Exception.Create(ACBrStr( Format(SErroCTeAbrir, [AValue]) ) );
end;

procedure TDFeMetodoCTe.CarregarDFeXML(const AValue: String);
begin
  if not ( TACBrCTe(FpACBrDFe).Conhecimentos.LoadFromString( AValue ) ) then
    raise Exception.Create(ACBrStr(SErroCteCarregar) );
end;

function TDFeMetodoCTe.ValidarDFe( const AValue: String ): Boolean;
begin
  Result := False;
  if ( TACBrCTe(FpACBrDFe).Conhecimentos.Count > 0 ) then
    Result:= True;

  if EstaVazio( FPathDFe )then
    FPathDFe := PathWithDelim(TACBrCTe(FpACBrDFe).Configuracoes.Arquivos.PathSalvar)
                + AValue;

  if EstaVazio( FPathDFeExtensao )then
    FPathDFeExtensao := PathWithDelim(TACBrCTe(FpACBrDFe).Configuracoes.Arquivos.PathSalvar)
                        + AValue + CExtensaoXmlCTe ;
end;

constructor TDFeMetodoCTe.Create(AACBrDFe: TACBrCTe; AXMLorFile: String);
begin
  inherited Create(AACBrDFe, AXMLorFile);
end;

destructor TDFeMetodoCTe.Destroy;
begin
  inherited;
end;

{ TDFeMetodoMDFeEvento }

procedure TDFeMetodoMDFeEvento.CarregarDFePath( const AValue: String);
begin
  if not ( TACBrMDFe(FpACBrDFe).EventoMDFe.LerXML( AValue ) ) then
    raise Exception.Create(ACBrStr( Format(SErroMDFeAbrir, [AValue]) ) );
end;

procedure TDFeMetodoMDFeEvento.CarregarDFeXML(const AValue: String);
begin
  if not ( TACBrMDFe(FpACBrDFe).EventoMDFe.LerXMLFromString( AValue ) ) then
    raise Exception.Create(ACBrStr(SErroMDFeCarregar) );
end;

function TDFeMetodoMDFeEvento.ValidarDFe( const AValue: String ): Boolean;
begin
  Result := False;
  if ( TACBrMDFe(FpACBrDFe).EventoMDFe.Evento.Count > 0 ) then
    Result:= True;

  if EstaVazio( FPathDFe )then
    FPathDFe := PathWithDelim(TACBrMDFe(FpACBrDFe).Configuracoes.Arquivos.PathSalvar)
                + AValue;

  if EstaVazio( FPathDFeExtensao )then
    FPathDFeExtensao := PathWithDelim(TACBrMDFe(FpACBrDFe).Configuracoes.Arquivos.PathSalvar)
                        + AValue + CExtensaoXmlMdfeEve ;
end;

constructor TDFeMetodoMDFeEvento.Create(AACBrDFe: TACBrMDFe; AXMLorFile: String);
begin
  inherited Create(AACBrDFe, AXMLorFile);
end;

destructor TDFeMetodoMDFeEvento.Destroy;
begin
  inherited;
end;

{ TDFeMetodoMDFe }

procedure TDFeMetodoMDFe.CarregarDFePath( const AValue: String);
begin
  if not ( TACBrMDFe(FpACBrDFe).Manifestos.LoadFromFile( AValue ) ) then
    raise Exception.Create(ACBrStr( Format(SErroMDFeAbrir, [AValue]) ) );
end;

procedure TDFeMetodoMDFe.CarregarDFeXML(const AValue: String);
begin
  if not ( TACBrMDFe(FpACBrDFe).Manifestos.LoadFromString( AValue ) ) then
    raise Exception.Create(ACBrStr(SErroMDFeCarregar) );
end;

function TDFeMetodoMDFe.ValidarDFe( const AValue: String ): Boolean;
begin
  Result := False;
  if ( TACBrMDFe(FpACBrDFe).Manifestos.Count > 0 ) then
    Result:= True;

  if EstaVazio( FPathDFe )then
    FPathDFe := PathWithDelim(TACBrMDFe(FpACBrDFe).Configuracoes.Arquivos.PathSalvar)
                + AValue;

  if EstaVazio( FPathDFeExtensao )then
    FPathDFeExtensao := PathWithDelim(TACBrMDFe(FpACBrDFe).Configuracoes.Arquivos.PathSalvar)
                        + AValue + CExtensaoXmlMdfe ;
end;

constructor TDFeMetodoMDFe.Create(AACBrDFe: TACBrMDFe; AXMLorFile: String);
begin
  inherited Create(AACBrDFe, AXMLorFile);
end;

destructor TDFeMetodoMDFe.Destroy;
begin
  inherited;
end;

end.

