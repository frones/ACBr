{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2004 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Isaque Pinheiro                                 }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{ Esse arquivo usa a classe  SynaSer   Copyright (c)2001-2003, Lukas Gebauer   }
{  Project : Ararat Synapse     (Found at URL: http://www.ararat.cz/synapse/)  }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* 27/10/2011: Primeira Versao
|*    Isaque Pinheiro e Daniel Simoes de Almeida
|*    Criaçao do componente ACBrDownload, que implementa de fazer download de
|*    arquivos via http e ftp, com recurso de pausa e continuação do download.
******************************************************************************}

{$I ACBr.inc}

unit ACBrDownload;

interface

uses
  SysUtils, Classes,
  ACBrBase, ACBrDownloadClass ;

const
   CACBrDownload_Versao = '1.0.0';

type
  TACBrProtocolo = (protNenhum, protHTTP, protFTP);
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrDownload = class(TACBrComponent)
  private
    { Private declarations }
    fDown: TACBrDownloadClass;
    fProtocolo: TACBrProtocolo;
    fDownloadDest: string;
    fDownloadStatus: TDownloadStatus;
    fDownloadUrl: string;
    fDownloadNomeArq : string;
    fProxy: TACBrProxy;
    fSizeRecvBuffer: Integer;
    fFTP: TACBrFTP;

    fOnBeforeDownload: TACBrBeforeDownload;
    fOnHookStatus: TACBrHookStatus;
    fOnDownloadStatus: TACBrDownloadStatus;
    fOnAfterDownload: TACBrAfterDownload;
    fOnHookMonitor: TACBrHookMonitor;

    procedure SetDownloadDest(const Value: string);
    procedure SetDownloadStatus(const Value: TDownloadStatus);
    procedure SetDownloadUrl(const Value: string);
    procedure SetDownloadNomeArq(const Value:string); 
    procedure SetFTP(const Value: TACBrFTP);
    procedure SetProxy(const Value: TACBrProxy);
    procedure SetSizeRecvBuffer(const Value: Integer);
    procedure SetOnAfterDownload(const Value: TACBrAfterDownload);
    procedure SetOnBeforeDownload(const Value: TACBrBeforeDownload);
    procedure SetOnDownloadStatus(const Value: TACBrDownloadStatus);
    procedure SetOnHookMonitor(const Value: TACBrHookMonitor);
    procedure SetOnHookStatus(const Value: TACBrHookStatus);
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure StartDownload;

    property DownloadStatus: TDownloadStatus read fDownloadStatus write SetDownloadStatus;
  published
    { Published declarations }
    property DownloadDest: string read fDownloadDest write SetDownloadDest;
    property DownloadUrl: string read fDownloadUrl write SetDownloadUrl;
    property DownloadNomeArq : string read fDownloadNomeArq write SetDownloadNomeArq;
    property SizeRecvBuffer: Integer read fSizeRecvBuffer write SetSizeRecvBuffer;
    property Proxy: TACBrProxy read fProxy write SetProxy;
    property FTP: TACBrFTP read fFTP write SetFTP;
    property Protocolo: TACBrProtocolo read fProtocolo write fProtocolo default protNenhum;
    // Eventos
    property OnBeforeDownload: TACBrBeforeDownload read fOnBeforeDownload write SetOnBeforeDownload;
    property OnAfterDownload: TACBrAfterDownload read fOnAfterDownload write SetOnAfterDownload;
    property OnHookStatus: TACBrHookStatus read fOnHookStatus write SetOnHookStatus;
    property OnHookMonitor: TACBrHookMonitor read fOnHookMonitor write SetOnHookMonitor;
    property OnDownloadStatus: TACBrDownloadStatus read fOnDownloadStatus write SetOnDownloadStatus;
  end;

implementation

Uses ACBrHTTPDownload, ACBrFTPDownload ;

{$IFNDEF FPC}
 {$R ACBrDownload.dcr}
{$ENDIF}

{ TACBrDownload }

constructor TACBrDownload.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fDown  := TACBrDownloadClass.Create(Self);
  fProxy := TACBrProxy.Create;
  fFTP   := TACBrFTP.Create;

  fDownloadDest   := '';
  fDownloadUrl    := '';
  fDownloadNomeArq := '';
  fDownloadStatus := stNone;
  fProtocolo      := protNenhum;
end;

destructor TACBrDownload.Destroy;
begin
  // Esta class tem que ser liberada antes das outras.
  FreeAndNil(fDown);

  fProxy.Free;
  fFTP.Free;
  inherited Destroy;
end;

procedure TACBrDownload.SetDownloadDest(const Value: string);
begin
  fDownloadDest := Value;
  fDown.DownloadDest := Value;
end;

procedure TACBrDownload.SetDownloadStatus(const Value: TDownloadStatus);
begin
  fDownloadStatus := Value;
  fDown.DownloadStatus := Value;
end;

procedure TACBrDownload.SetDownloadUrl(const Value: string);
begin
  fDownloadUrl := Value;
  fDown.DownloadUrl := Value;
end;

procedure TACBrDownload.SetDownloadNomeArq(const Value: string);
begin
  fDownloadNomeArq := Value;
  fDown.DownloadNomeArq := Value;
end;


procedure TACBrDownload.SetFTP(const Value: TACBrFTP);
begin
  fFTP := Value;
  if fProtocolo = protFTP then
     TACBrFTPDownload(fDown).FTP := Value;
end;

procedure TACBrDownload.SetOnAfterDownload(const Value: TACBrAfterDownload);
begin
  fOnAfterDownload := Value;
  fDown.OnAfterDownload := Value;
end;

procedure TACBrDownload.SetOnBeforeDownload(const Value: TACBrBeforeDownload);
begin
  fOnBeforeDownload := Value;
  fDown.OnBeforeDownload := Value;
end;

procedure TACBrDownload.SetOnDownloadStatus(const Value: TACBrDownloadStatus);
begin
  fOnDownloadStatus := Value;
  fDown.OnDownloadStatus := Value;
end;

procedure TACBrDownload.SetOnHookMonitor(const Value: TACBrHookMonitor);
begin
  fOnHookMonitor := Value;
  fDown.OnHookMonitor := Value;
end;

procedure TACBrDownload.SetOnHookStatus(const Value: TACBrHookStatus);
begin
  fOnHookStatus := Value;
  fDown.OnHookStatus := Value;
end;

procedure TACBrDownload.SetProxy(const Value: TACBrProxy);
begin
  fProxy := Value;
  fDown.Proxy := Value;
end;

procedure TACBrDownload.SetSizeRecvBuffer(const Value: Integer);
begin
  fSizeRecvBuffer := Value;
  fDown.SizeRecvBuffer := Value;
end;

procedure TACBrDownload.StartDownload;
begin
   if not (fDown is TACBrDownloadClass) then
      raise Exception.Create('Antes de iniciar o download, defina o protocolo a ser usado HTTP ou FTP!');

   if Assigned(fDown) then
      FreeAndNil(fDown);

   case fProtocolo of
     protHTTP: fDown := TACBrHTTPDownload.Create(Self);
     protFTP : fDown := TACBrFTPDownload.Create(Self);
   end;

   fDownloadStatus := stDownload;

   if fProtocolo <> protNenhum then
      fDown.StartDownload;
end;

end.
