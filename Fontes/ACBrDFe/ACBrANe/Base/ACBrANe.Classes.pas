{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Giurizzato Junior                         }
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

unit ACBrANe.Classes;

interface

uses
  SysUtils, Classes,
  {$IFNDEF VER130}
    Variants,
  {$ENDIF}
  {$IF DEFINED(NEXTGEN)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$IFEND}
  ACBrBase,
  ACBrUtil.DateTime,
  ACBrUtil.Strings,
  ACBrANe.Conversao;

type

  TANe = class(TObject)
  private
    /////// ATM
    Fusuario: string;
    Fsenha: string;
    Fcodatm: string;

    /////// ELT
    FNomeArq: string;
    FCNPJ: string;

    /////// ATM e ELT
    FxmlDFe: string;

    Faplicacao: string;
    Fassunto: string;
    Fremetentes: string;
    Fdestinatarios: string;
    Fcorpo: string;
    Fchave: string;
    Fchaveresp: string;

  public
    /////// ATM
    property usuario: string  read Fusuario  write Fusuario;
    property senha: string    read Fsenha    write Fsenha;
    property codatm: string   read Fcodatm   write Fcodatm;

    /////// ELT
    property NomeArq: string  read FNomeArq write FNomeArq;
    property CNPJ: string     read FCNPJ    write FCNPJ;

    /////// ATM e ELT
    // A propriedade abaixo é utilizada pelos serviços: AverbaNFe, AverbaCTe e DeclaraMDFe
    property xmlDFe: string   read FxmlDFe   write FxmlDFe;

    // As propriedades abaixo são utilizadas para o serviço AddBackMail
    property aplicacao: string     read Faplicacao     write Faplicacao;
    property assunto: string       read Fassunto       write Fassunto;
    property remetentes: string    read Fremetentes    write Fremetentes;
    property destinatarios: string read Fdestinatarios write Fdestinatarios;
    property corpo: string         read Fcorpo         write Fcorpo;
    property chave: string         read Fchave         write Fchave;
    property chaveresp: string     read Fchaveresp     write Fchaveresp;
  end;

implementation

end.
