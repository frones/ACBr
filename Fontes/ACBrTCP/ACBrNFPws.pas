{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:  André Ferreira de Morais                       }
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

{******************************************************************************
|* Historico
|*
|* 28/01/2013: Primeira Versao
|*    Daniel Simoes de Almeida e André Moraes
******************************************************************************}

unit ACBrNFPws ;

{$I ACBr.inc}

interface

uses
  Classes, SysUtils,
  ACBrBase, ACBrSocket;

const
  CNFPws_URLNF = 'https://www.nfp.fazenda.sp.gov.br/ws/arquivonf_mod1.asmx' ;
  CNFPws_URLCF = 'https://www.nfp.fazenda.sp.gov.br/ws/arquivocf.asmx' ;

type

  EACBrNFPwsException = class ( Exception );

  TACBrNFPwsCategoriaUsuario = (nfpNenhum, nfpContribuinte, nfpContabilista,
                                nfpConsumidor) ;

  TACBrNFPwsTipoDocto = (docCupomFiscal, docNF_Mod1) ;

  { TACBrNFPws }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrNFPws = class( TACBrHTTP )
    private
      fCategoriaUsuario: TACBrNFPwsCategoriaUsuario;
      fCNPJ: String;
      fModoTeste: Boolean;
      fOnBuscaEfetuada : TNotifyEvent ;
      fSenha: String;
      fTipoDocto: TACBrNFPwsTipoDocto;
      fURL: String;
      fUsuario: String;
      procedure SetTipoDocto(AValue: TACBrNFPwsTipoDocto);
    protected
      function SoapEnvelope( const Body: String ) : String ;
      procedure DoPOST( const SoapBody: String);

    public
      constructor Create(AOwner: TComponent); override;
      Destructor Destroy ; override ;

    published
      property URL     : String read fURL     write fURL ;
      property Usuario : String read fUsuario write fUsuario ;
      property Senha   : String read fSenha   write fSenha ;
      property CNPJ    : String read fCNPJ    write fCNPJ ;
      property CategoriaUsuario : TACBrNFPwsCategoriaUsuario
         read fCategoriaUsuario write fCategoriaUsuario;
      property ModoTeste: Boolean read fModoTeste write fModoTeste ;
      property TipoDocto: TACBrNFPwsTipoDocto read fTipoDocto write SetTipoDocto ;

      property OnBuscaEfetuada : TNotifyEvent read fOnBuscaEfetuada
         write fOnBuscaEfetuada ;

      function Consultar( const Protocolo: String ): String;
      function Enviar( const NomeArquivo: String; const Observacoes: String = '' ): String;
  end ;

implementation

uses ACBrUtil, strutils, ssl_openssl ;

constructor TACBrNFPws.Create(AOwner : TComponent) ;
begin
  inherited Create(AOwner) ;

  ParseText := False;

  fOnBuscaEfetuada := nil ;
  fURL             := CNFPws_URLCF;
  fTipoDocto       := docCupomFiscal;
  fUsuario         := '';
  fSenha           := '';
  fCNPJ            := '';
  fCategoriaUsuario:= nfpNenhum;
  fModoTeste       := True;
end ;

destructor TACBrNFPws.Destroy;
begin
  inherited Destroy ;
end ;

procedure TACBrNFPws.SetTipoDocto(AValue: TACBrNFPwsTipoDocto);
begin
  fTipoDocto:=AValue;

  if fTipoDocto = docNF_Mod1 then
     URL := CNFPws_URLNF
  else
     URL := CNFPws_URLCF;
end;

function TACBrNFPws.SoapEnvelope(const Body: String): String;
begin
  Result :=
  '<?xml version="1.0" encoding="utf-8"?>' +
  '<soap12:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" '+
    'xmlns:xsd="http://www.w3.org/2001/XMLSchema" '+
    'xmlns:soap12="http://www.w3.org/2003/05/soap-envelope">'+
    '<soap12:Header>'+
      '<Autenticacao Usuario="'+trim(Usuario)+
        '" Senha="'+trim(Senha)+
        '" CNPJ="'+trim(CNPJ)+
        '" CategoriaUsuario="'+IntToStr(Integer(CategoriaUsuario))+
        '" xmlns="https://www.nfp.sp.gov.br/ws" />'+
    '</soap12:Header>'+
    '<soap12:Body>'+
      Body+
    '</soap12:Body>'+
  '</soap12:Envelope>' ;
end;

procedure TACBrNFPws.DoPOST(const SoapBody: String);
var
  PostData: AnsiString;
begin
  PostData := SoapEnvelope( SoapBody ) ;

  HTTPSend.Clear;
  HTTPSend.Protocol := '1.1';
  HTTPSend.MimeType := 'application/soap+xml; charset=utf-8';

  HTTPSend.Document.Write(Pointer(PostData)^,Length(PostData));
  HTTPPost( URL );
end;


function TACBrNFPws.Consultar(const Protocolo: String): String;
begin
  DoPOST( '<Consultar xmlns="https://www.nfp.sp.gov.br/ws">'+
          '<Protocolo>'+Protocolo+'</Protocolo>'+
          '</Consultar>' ) ;

  Result := LerTagXML( RespHTTP.Text, 'ConsultarResult' ) ;
end;

function TACBrNFPws.Enviar(const NomeArquivo: String; const Observacoes: String
  ): String;
Var
  SL : TStringList ;
begin
  if not FileExists( NomeArquivo ) then
    raise EACBrNFPwsException.Create( ACBrStr('Arquivo ' + NomeArquivo + 'não encontrado') );

  SL := TStringList.Create;
  try
    SL.LoadFromFile( NomeArquivo );

    DoPOST( '<Enviar xmlns="https://www.nfp.sp.gov.br/ws">'+
              '<NomeArquivo>' + ExtractFileName(NomeArquivo) + '</NomeArquivo>'+
              '<EnvioNormal>'+IfThen(ModoTeste,'false','true')+'</EnvioNormal>'+
              '<ConteudoArquivo>' +
                ACBrUtil.ParseText( SL.Text, False, False )+
              '</ConteudoArquivo>'+
              '<Observacoes>'+Trim(Observacoes)+'</Observacoes>'+
            '</Enviar>' );

  finally
    SL.Free;
  end ;

  Result := LerTagXML( RespHTTP.Text, 'EnviarResult' ) ;
end;

end.

