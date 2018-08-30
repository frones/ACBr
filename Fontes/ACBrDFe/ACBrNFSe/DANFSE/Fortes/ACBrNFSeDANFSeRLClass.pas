{******************************************************************************}
{ Projeto: Componente ACBrNFSe                                                 }
{ Biblioteca multiplataforma de componentes Delphi para                        }
{ Emissão de Nota Fiscal de Serviço                                            }
{                                                                              }
{ Direitos Autorais Reservados (c) 2015 Italo Jurisato Junior                  }
{                                       Daniel Simoes de Almeida               }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{$I ACBr.inc}

unit ACBrNFSeDANFSeRLClass;

interface

uses
  Forms, SysUtils, Classes, pnfsNFSe, ACBrNFSeDANFSeClass;

type
	{$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}	
  TACBrNFSeDANFSeRL = class(TACBrNFSeDANFSeClass)
  private

  protected
    FPrintDialog: boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ImprimirDANFSe(NFSe: TNFSe = nil); override;
    procedure ImprimirDANFSePDF(NFSe: TNFSe = nil); override;
  published
    property PrintDialog: boolean read FPrintDialog write FPrintDialog;
  end;

implementation

uses
  StrUtils, Dialogs, ACBrUtil, ACBrNFSe, ACBrNFSeDANFSeRLRetrato;

constructor TACBrNFSeDANFSeRL.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPrintDialog := True;
end;

destructor TACBrNFSeDANFSeRL.Destroy;
begin
  inherited Destroy;
end;

procedure TACBrNFSeDANFSeRL.ImprimirDANFSe(NFSe: TNFSe = nil);
var
  i: integer;
  frlDANFSeRLRetrato: TfrlDANFSeRLRetrato;
begin
  frlDANFSeRLRetrato := TfrlDANFSeRLRetrato.Create(Self);
  try
    frlDANFSeRLRetrato.QuebradeLinha(TACBrNFSe(ACBrNFSe).Configuracoes.WebServices.QuebradeLinha);
		if NFSe = nil then
		begin
		  for i := 0 to TACBrNFSe(ACBrNFSe).NotasFiscais.Count - 1 do
			begin
			  frlDANFSeRLRetrato.Imprimir(Self,
          TACBrNFSe(ACBrNFSe).NotasFiscais.Items[i].NFSe
          , Logo
          , Email
          , Fax
          , NumCopias
          , Sistema
          , Site
          , Usuario
          , MostrarPreview
          , MargemSuperior
          , MargemInferior
          , MargemEsquerda
          , MargemDireita
          , Impressora
          , PrestLogo
          , Prefeitura
          , RazaoSocial
          , Endereco
          , Complemento
          , Fone
          , Municipio
          , InscMunicipal
          , EMail_Prestador
          , UF
          , T_InscEstadual
          , T_InscMunicipal
          , OutrasInformacaoesImp
          , Atividade
          , T_Fone
          , T_Endereco
          , T_Complemento
          , T_Email
          , PrintDialog
          , ImprimeCanhoto
          , DetalharServico);
			  end;
		  end
		  else
			  frlDANFSeRLRetrato.Imprimir(Self,
          NFSe
          , Logo
          , Email
          , Fax
          , NumCopias
          , Sistema
          , Site
          , Usuario
          , MostrarPreview
          , MargemSuperior
          , MargemInferior
          , MargemEsquerda
          , MargemDireita
          , Impressora
          , PrestLogo
          , Prefeitura
          , RazaoSocial
          , Endereco
          , Complemento
          , Fone
          , Municipio
          , InscMunicipal
          , EMail_Prestador
          , UF
          , T_InscEstadual
          , T_InscMunicipal
          , OutrasInformacaoesImp
          , Atividade
          , T_Fone
          , T_Endereco
          , T_Complemento
          , T_Email
          , PrintDialog
          , ImprimeCanhoto
          , DetalharServico);
	 finally
		  frlDANFSeRLRetrato.Free;
	 end;
end;

procedure TACBrNFSeDANFSeRL.ImprimirDANFSePDF(NFSe: TNFSe = nil);
var
  NomeArq: string;
  i: integer;
  frlDANFSeRLRetrato: TfrlDANFSeRLRetrato;
begin
  frlDANFSeRLRetrato := TfrlDANFSeRLRetrato.Create(Self);
  try
	  frlDANFSeRLRetrato.QuebradeLinha(TACBrNFSe(ACBrNFSe).Configuracoes.WebServices.QuebradeLinha);
	  if NFSe = nil then
	  begin
      for i := 0 to TACBrNFSe(ACBrNFSe).NotasFiscais.Count - 1 do
      begin
        NomeArq := PathWithDelim(Self.PathPDF) + TACBrNFSe(ACBrNFSe).NumID[TACBrNFSe(ACBrNFSe).NotasFiscais.Items[i].NFSe] + '-nfse.pdf';

				frlDANFSeRLRetrato.SavePDF(Self
          , NomeArq
          , TACBrNFSe(ACBrNFSe).NotasFiscais.Items[i].NFSe
          , Logo
          , Email
          , Fax
          , NumCopias
          , Sistema
          , Site
          , Usuario
          , MargemSuperior
          , MargemInferior
          , MargemEsquerda
          , MargemDireita
          , PrestLogo
          , Prefeitura
          , RazaoSocial
          , Endereco
          , Complemento
          , Fone
          , Municipio
          , InscMunicipal
          , EMail_Prestador
          , UF
          , T_InscEstadual
          , T_InscMunicipal
          , OutrasInformacaoesImp
          , Atividade
          , T_Fone
          , T_Endereco
          , T_Complemento
          , T_Email
          , ImprimeCanhoto
          , DetalharServico);
      end;
	  end
	  else
	  begin
      NomeArq := PathWithDelim(Self.PathPDF) + TACBrNFSe(ACBrNFSe).NumID[NFSe] + '-nfse.pdf';

		  frlDANFSeRLRetrato.SavePDF(Self
        , NomeArq
        , NFSe
        , Logo
        , Email
        , Fax
        , NumCopias
        , Sistema
        , Site
        , Usuario
        , MargemSuperior
        , MargemInferior
        , MargemEsquerda
        , MargemDireita
        , PrestLogo
        , Prefeitura
        , RazaoSocial
        , Endereco
        , Complemento
        , Fone
        , Municipio
        , InscMunicipal
        , EMail_Prestador
        , UF
        , T_InscEstadual
        , T_InscMunicipal
        , OutrasInformacaoesImp
        , Atividade
        , T_Fone
        , T_Endereco
        , T_Complemento
        , T_Email
        , ImprimeCanhoto
        , DetalharServico);
	  end;
  finally
	  frlDANFSeRLRetrato.Free;
  end;
end;

end.
