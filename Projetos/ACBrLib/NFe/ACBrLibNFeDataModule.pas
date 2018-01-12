unit ACBrLibNFeDataModule;

{$mode delphi}

interface

uses
  Classes, SysUtils, syncobjs, FileUtil, ACBrUtil, ACBrNFe, ACBrMail, IniFiles,
  ACBrPosPrinter, ACBrNFeDANFeRLClass, ACBrDANFCeFortesFr, ACBrNFeDANFeESCPOS;

type

  { TNFeDataModule }

  TNFeDataModule = class(TDataModule)
    ACBrMail: TACBrMail;
    ACBrNFe: TACBrNFe;
    ACBrNFeDANFCeFortes: TACBrNFeDANFCeFortes;
    ACBrNFeDANFeESCPOS: TACBrNFeDANFeESCPOS;
    ACBrNFeDANFeRL: TACBrNFeDANFeRL;
    ACBrPosPrinter: TACBrPosPrinter;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);

  private
    FPass: String;
    FUltimoErro: String;
    FStatusServico: String;
    FACBrNFeINI: String;
    FLock: TCriticalSection;
  public
    property StatusServico: String read FStatusServico write FStatusServico;
    property Lock: TCriticalSection read FLock;

    procedure AplicarConfiguracoes;
  end;

implementation

Uses
  ACBrLibConfig, ACBrLibNFeConfig;

{$R *.lfm}

{ TNFeDataModule }

procedure TNFeDataModule.DataModuleCreate(Sender: TObject);
begin
  FLock := TCriticalSection.Create;
  FUltimoErro := '';
  FACBrNFeINI := ApplicationPath + 'ACBrNFe.ini';
  FPass := 'tYk*5W@';
end;

procedure TNFeDataModule.DataModuleDestroy(Sender: TObject);
begin
  FLock.Destroy;
end;

procedure TNFeDataModule.AplicarConfiguracoes;
begin
  VerificarLibConfigFoiInstaciado;

  ACBrNFe.SSL.DescarregarCertificado;

  ACBrNFe.Configuracoes.Assign( TLibNFeConfig( pLibConfig ).NFeConfig );

  with ACBrMail do
  begin
    FromName := pLibConfig.Email.Nome;
    From := pLibConfig.Email.Conta;
    Host := pLibConfig.Email.Servidor;
    Port := IntToStr( pLibConfig.Email.Porta );
    Username := pLibConfig.Email.Usuario;
    Password := pLibConfig.Email.Senha;
    SetSSL := pLibConfig.Email.SSL;
    SetTLS := pLibConfig.Email.TLS;
    ReadingConfirmation := pLibConfig.Email.Confirmacao;
    UseThread := pLibConfig.Email.SegundoPlano;
    DefaultCharset := pLibConfig.Email.Codificacao;
  end;

end;

end.
