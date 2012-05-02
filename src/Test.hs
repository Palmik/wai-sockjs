import qualified Network.Wai.Application.Sock as SHS
import           Network.Wai.Handler.Warp

main = run 8000 SHS.dispatcher