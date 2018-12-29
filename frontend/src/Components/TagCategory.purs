module Components.TagCategory where

import Prelude

import Components.Tag (TagMessage(..))
import Components.Tag as Tag
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Models.Tag as TagModel

type TagCategoryState =
  { tagCategory :: TagModel.TagCategory }

data TagCategoryQuery a
  = UpdateCategory TagModel.TagCategory a
  | HandleCreateButton a
  | HandleTag TagMessage a

data TagCategorySlot = TagSlot Number
derive instance eqSlot :: Eq TagCategorySlot
derive instance ordSlot :: Ord TagCategorySlot

type TagCategoryInput = TagModel.TagCategory

data TagCategoryMessage
  = CategoryTagRemove TagModel.Tag
  | CategoryTagSelect TagModel.Tag
  | CategoryTagCreate

view :: forall m. TagModel.TagCategory -> H.Component HH.HTML TagCategoryQuery TagCategoryInput TagCategoryMessage m
view tagCategory =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: HE.input UpdateCategory }
  where
    initialState :: TagCategoryState
    initialState =
      { tagCategory }

    render :: TagCategoryState -> H.ParentHTML TagCategoryQuery Tag.TagQuery TagCategorySlot m
    render state =
      HH.div
        [ HP.class_ (H.ClassName "card")]
        [ header
        , HH.section [HP.class_ (H.ClassName "card__body")] tagSlots
        , footer
        ]
      where
        header =
          HH.header
            [ HP.class_ (H.ClassName "card__header flex__box")]
            [ HH.p [ HP.class_ (H.ClassName "card__title flex__item") ] [ HH.text (TagModel.getCategoryTitle state.tagCategory) ]
            , HH.button [ HE.onClick (HE.input_ HandleCreateButton) ] [ HH.text "New Tag" ] ]
        tagSlot tag = HH.slot (TagSlot (TagModel.getId tag)) (Tag.view tag) tag (HE.input HandleTag)
        tagSlots = map tagSlot (TagModel.getItems (TagModel.getCategoryTags state.tagCategory))
        footer =
          HH.footer
            [ HP.class_ (H.ClassName "card__footer flex__box") ]
            [ HH.p [HP.class_ (H.ClassName "flex__item")] [HH.text ("Total: " <> "0")], HH.text "More"]

    eval :: TagCategoryQuery ~> H.ParentDSL TagCategoryState TagCategoryQuery Tag.TagQuery TagCategorySlot TagCategoryMessage m
    eval = case _ of
      UpdateCategory tagCategory next -> do
        H.put { tagCategory }
        pure next
      HandleCreateButton next -> do
        H.raise $ CategoryTagCreate
        pure next
      HandleTag (TagSelect tag) next -> do
        H.raise $ CategoryTagSelect tag
        pure next
      HandleTag (TagRemove tag) next -> do
        H.raise $ CategoryTagRemove tag
        pure next
