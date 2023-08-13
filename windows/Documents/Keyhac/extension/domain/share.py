from abc import (
    ABCMeta,
    abstractmethod,
)


class AbstractMeta(ABCMeta):
    """
    Metaclass for abstract class

    Do not allow override methods of abstract class
    """
    __SENTINEL = object()

    def __new__(cls, name, bases, class_dict):
        private = {key
                   for base in bases
                   for key, value in vars(base).items()
                   if callable(value) and cls.__is_final(value)}
        if any(key in private for key in class_dict):
            raise RuntimeError('certain methods may not be overridden')
        return super().__new__(cls, name, bases, class_dict)

    @classmethod
    def __is_final(cls, method):
        try:
            return method.__final is cls.__SENTINEL
        except AttributeError:
            return False

    @classmethod
    def final(cls, method):
        """派生クラスでオーバーロードを許可しないメソッドをデコレーションする"""
        method.__final = cls.__SENTINEL
        return method


class Entity(metaclass=AbstractMeta):
    """
    ドメインレイヤーの構成要素で、
    アプリケーション上で同一性の識別が必要なオブジェクト
    """
    @abstractmethod
    def __eq__(self, other) -> bool:
        raise NotImplementedError

    @abstractmethod
    def __hash__(self) -> int:
        raise NotImplementedError


class ValueObject(metaclass=AbstractMeta):
    """
    ドメインレイヤーの構成要素で、
    ドメイン上に登場する値そのもの格納する、同一性を持たないオブジェクト
    """
    @AbstractMeta.final
    def __eq__(self, other) -> bool:
        if type(self) != type(other):
            return False
        return hash(self) == hash(other)

    @abstractmethod
    def __hash__(self) -> int:
        raise NotImplementedError


class Service(metaclass=AbstractMeta):
    """
    ドメインレイヤーの構成要素で、EntityやValueObjectに属さないオブジェクト
    """
    pass


class Factory(metaclass=AbstractMeta):
    """
    集約の生成処理をカプセル化するオブジェクト
    """
    pass


class Repository(metaclass=AbstractMeta):
    """
    DB等への永続化処理をドメインモデルと切り離すための、
    永続化／問い合わせ専用オブジェクト
    """
    pass
