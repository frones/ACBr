{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2023 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Jaques Nascimento                               }
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

unit ACBr.Android.VideoPlayer;

{$I ACBr.inc}

interface

Uses
   ACBrBase,
   {$IFDEF ANDROID}
   FMX.Helpers.Android,
   FMX.Platform.Android,
   Androidapi.Helpers,
   ACBr.Androidapi.JNI,
   Androidapi.JNI.GraphicsContentViewText,
   Androidapi.JNI.JavaTypes,
   Androidapi.JNI.Hardware,
   Androidapi.JNI.Embarcadero,
   Androidapi.JNI.Media,
   Androidapi.JNI.App,
   Androidapi.JNI.Os,
   Androidapi.JNI.Util,
   {$ENDIF}
   System.Types,
   System.UITypes,
   System.Classes,
   System.SysUtils,
   System.Math,
   System.Messaging,
   System.Generics.Collections,
   FMX.Platform,
   FMX.Types,
   FMX.Forms,
   FMX.Controls,
   FMX.Layouts,
   FMX.Surfaces,
   FMX.Graphics;


Type

   [ComponentPlatformsAttribute (piacbrAllAndroidPlatforms)]
   TACBrVideoPlayer = Class(TComponent)
      Private
         {$IFDEF ANDROID}
         FPlay : JVideoPlayer;
         {$ENDIF}
         FPath : String;
         FShow : Boolean;
      Public
         Constructor Create(aOwner : TComponent); Override;
         Procedure Show;
         Procedure Hide;
         Procedure Play;
         Procedure Pause;
         Procedure Resume;
         Procedure SetPathAndFile(aPath : String);
         Procedure SetBounds(x, y, w, h : Integer);
         Property IsSHow : Boolean Read FShow;
      Published
         Property PathAndFile : String Read FPath Write SetPathAndFile;
      End;


implementation

{ TVideoPlayer }

constructor TACBrVideoPlayer.Create(aOwner: TComponent);
begin
inherited;
{$IFDEF ANDROID}
FPlay := TJVideoPlayer.javaClass.init(MainActivity);
{$ENDIF}
FShow := False;
end;

procedure TACBrVideoPlayer.Show;
begin
{$IFDEF ANDROID}
FPlay.Show;
FShow := True;
{$ENDIF}
end;

procedure TACBrVideoPlayer.Hide;
begin
{$IFDEF ANDROID}
FPlay.Hide;
FShow := False;
{$ENDIF}
end;

procedure TACBrVideoPlayer.Pause;
begin
{$IFDEF ANDROID}
FPlay.Pause;
{$ENDIF}
end;

procedure TACBrVideoPlayer.Play;
begin
{$IFDEF ANDROID}
if Not IsShow then
   Show;
FPlay.Play;
{$ENDIF}
end;

procedure TACBrVideoPlayer.Resume;
begin
{$IFDEF ANDROID}
FPlay.Resume;
{$ENDIF}
end;

procedure TACBrVideoPlayer.SetBounds(x, y, w, h: Integer);
begin
{$IFDEF ANDROID}
FPlay.SetBounds(x, y, w, h);
{$ENDIF}
end;

procedure TACBrVideoPlayer.SetPathAndFile(aPath: String);
begin
FPath := aPath;
{$IFDEF ANDROID}
//   /sdcard/Movies/video_02.mp4
if Not IsShow then
   Show;
FPlay.setPath(StringToJString(aPath));
{$ENDIF}
end;

end.
