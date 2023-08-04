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
        method.__final = cls.__SENTINEL
        return method


class Entity(metaclass=AbstractMeta):
    @abstractmethod
    def __eq__(self, other) -> bool:
        raise NotImplementedError

    @abstractmethod
    def __hash__(self) -> int:
        raise NotImplementedError


class ValueObject(metaclass=AbstractMeta):
    @AbstractMeta.final
    def __eq__(self, other) -> bool:
        if type(self) != type(other):
            return False
        return hash(self) == hash(other)

    @abstractmethod
    def __hash__(self) -> int:
        raise NotImplementedError


class Service(metaclass=AbstractMeta):
    pass


class Factory(metaclass=AbstractMeta):
    pass


class Repository(metaclass=AbstractMeta):
    pass
