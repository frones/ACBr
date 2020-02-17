{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Jurisato Junior                           }
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

unit pcaANe;

interface

uses
  SysUtils,
{$IFNDEF VER130}
  Variants,
{$ENDIF}
  Classes;

type

  TANe = class(TObject)
  private
    /////// ATM
    Fusuario: String;
    Fsenha: String;
    Fcodatm: String;

    /////// ELT
    FNomeArq: String;
    FCNPJ: String;

    /////// ATM e ELT
    FxmlDFe: String;

    Faplicacao: String;
    Fassunto: String;
    Fremetentes: String;
    Fdestinatarios: String;
    Fcorpo: String;
    Fchave: String;
    Fchaveresp: String;

  public
    /////// ATM
    property usuario: String  read Fusuario  write Fusuario;
    property senha: String    read Fsenha    write Fsenha;
    property codatm: String   read Fcodatm   write Fcodatm;

    /////// ELT
    property NomeArq: String  read FNomeArq write FNomeArq;
    property CNPJ: String     read FCNPJ    write FCNPJ;

    /////// ATM e ELT
    // A propriedade abaixo é utilizada pelos serviços: AverbaNFe, AverbaCTe e DeclaraMDFe
    property xmlDFe: String   read FxmlDFe   write FxmlDFe;

    // As propriedades abaixo são utilizadas para o serviço AddBackMail
    property aplicacao: String     read Faplicacao     write Faplicacao;
    property assunto: String       read Fassunto       write Fassunto;
    property remetentes: String    read Fremetentes    write Fremetentes;
    property destinatarios: String read Fdestinatarios write Fdestinatarios;
    property corpo: String         read Fcorpo         write Fcorpo;
    property chave: String         read Fchave         write Fchave;
    property chaveresp: String     read Fchaveresp     write Fchaveresp;
  end;

implementation

end.

